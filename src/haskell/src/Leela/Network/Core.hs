{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE Rank2Types        #-}

-- Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Leela.Network.Core
       ( CoreServer ()
       , newCore
       , process
       ) where

import qualified Data.Map as M
import           Data.IORef
import           Leela.Logger
import           Leela.Helpers
import           Control.Monad
import           Leela.Data.LQL
import qualified Data.ByteString as B
import           Leela.Data.Graph
import           Control.Exception
import           Leela.Data.Naming
import           Control.Concurrent
import           Leela.Data.QDevice
import           Leela.Data.Endpoint
import           Leela.Data.LQL.Comp
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.UTF8 (fromString)
import           System.Random.Shuffle
import           Leela.Storage.Backend
import           Control.Concurrent.STM
import           Leela.Network.Protocol

data CoreServer = CoreServer { stat   :: IORef [(String, [Endpoint])]
                             , fdseq  :: TVar FH
                             , fdlist :: TVar (M.Map (B.ByteString, FH) (Int, Device Reply))
                             }

ttl :: Int
ttl = 300

dumpStat :: CoreServer -> IO [(B.ByteString, B.ByteString)]
dumpStat core = do
  state <- readIORef (stat core)
  shuffleM $ concatMap dumpEntry state
    where
      dumpEntry (k, [])     = [(fromString $ "endpoint/" ++ k, "")]
      dumpEntry (k, [e])    = [(fromString $ "endpoint/" ++ k, toStrict $ dumpEndpoint e)]
      dumpEntry (k, (e:es)) = (fromString $ "endpoint/" ++ k, toStrict $ dumpEndpoint e) : dumpEntry (k, es)

newCore :: IORef [(String, [Endpoint])] -> IO CoreServer
newCore statdb = do
  state <- makeState
  _     <- forkIO (forever (sleep 1 >> rungc (fdlist state)))
  return state
    where
      makeState = atomically $
        liftM2 (CoreServer statdb) (newTVar 0) (newTVar M.empty)

rungc :: (Ord k, Show k) => TVar (M.Map k (Int, Device a)) -> IO ()
rungc tvar = atomically kill >>= mapM_ burry
    where
      partition acc []       = acc
      partition (a, b) ((k, (tick, dev)):xs)
        | tick == 0 = partition ((k, dev) : a, b) xs
        | otherwise = partition (a, (k, (tick - 1, dev)) : b) xs

      kill = do
        (dead, alive) <- fmap (partition ([], []) . M.toList) (readTVar tvar)
        writeTVar tvar (M.fromList alive)
        return dead

      burry (k, dev) = do
        lwarn Network $ printf "closing/purging unused channel: %s" (show k)
        atomically $ close dev

nextfd :: CoreServer -> STM FH
nextfd srv = do
  curr <- readTVar $ fdseq srv
  writeTVar (fdseq srv) (curr + 1)
  return curr

makeFD :: CoreServer -> User -> IO (FH, Device Reply)
makeFD srv (User u) = atomically $ do
  ctrl <- control
  fd   <- nextfd srv
  dev  <- open ctrl 512
  modifyTVar (fdlist srv) (M.insert (u, fd) (ttl, dev))
  return (fd, dev)

selectFD :: CoreServer -> (User, FH) -> IO (Maybe (Device Reply))
selectFD srv ((User u), fh) = atomically $ do
  let resetTTL _ (_, dev) = Just (ttl, dev)
  (mdev, newv) <- fmap (M.updateLookupWithKey resetTTL (u, fh)) (readTVar (fdlist srv))
  writeTVar (fdlist srv) newv
  return (fmap snd mdev)

closeFD :: CoreServer -> Bool -> (User, FH) -> IO ()
closeFD srv nowait ((User u), fh) = do
  ldebug Network (printf "closing fd %s" (show k))
  atomically $ do
    db   <- readTVar (fdlist srv)
    case (M.lookup k db) of
      Nothing       -> return ()
      Just (_, dev) -> do unless nowait (linger dev)
                          writeTVar (fdlist srv) (M.delete k db)
                          close dev
    where
      k = (u, fh)

data Stream a = Chunk a
              | Error SomeException
              | EOF

thr :: (a, b, c) -> c
thr (_, _, c) = c

navigate :: (GraphBackend m) => m -> Device Reply -> (Matcher, [(GUID -> Matcher)]) -> IO ()
navigate db queue (source, pipeline) = do
  srcpipe <- openIO queue 16
  forkSource srcpipe
  dstpipe <- forkFilters srcpipe pipeline
  copy dstpipe asReply queue
    where
      two (_, b, c) = (c, b)

      asReply (Chunk (feed, path)) = Just (Item (List $ map (Path . (: path) . two) feed))
      asReply EOF                  = Nothing
      asReply (Error e)            = throw e

      runFilter srcpipe f dstpipe = do
        mg <- devreadIO srcpipe
        case mg of
          Nothing                   -> return ()
          Just (Chunk (feed, path)) -> do
            forM_ feed (\link -> 
              query db (devwriteIO dstpipe . Chunk . (, (two link) : path)) (f $ thr link))
            runFilter srcpipe f dstpipe
          Just chunk                -> devwriteIO dstpipe chunk

      forkSource dstpipe = void $ forkFinally
        (query db (devwriteIO dstpipe . Chunk . (, [])) source)
        (either (devwriteIO dstpipe . Error) (const $ devwriteIO dstpipe EOF))

      forkFilter srcpipe f = do
        dstpipe <- openIO srcpipe 16
        void $ forkFinally
          (runFilter srcpipe f dstpipe)
          (either (devwriteIO dstpipe . Error) (const $ devwriteIO dstpipe EOF))
        return dstpipe

      forkFilters srcpipipe []               = return srcpipipe
      forkFilters srcpipipe (f:fs) = do
        dstpipe <- forkFilter srcpipipe f
        forkFilters dstpipe fs

evalLQL :: (GraphBackend m) => m -> CoreServer -> Device Reply -> [LQL] -> IO ()
evalLQL _ _ queue []         = devwriteIO queue (Last Nothing)
evalLQL db core queue (x:xs) =
  case x of
    PathStmt q        -> navigate db queue q >> evalLQL db core queue xs
    NameStmt _ g      -> do
      (gUser, gTree, gName) <- getName db g
      devwriteIO queue (Item $ Name gUser gTree gName g)
      evalLQL db core queue xs
    StatStmt          -> do
      state <- dumpStat core
      devwriteIO queue (Item $ Stat state)
      evalLQL db core queue xs
    AlterStmt journal -> do
      names <- update db journal
      mapM_ (\(u, t, n, g) -> devwriteIO queue (Item $ Name u t n g)) names
      evalLQL db core queue xs
    GUIDStmt u n      -> do
      mg <- getGUID db (uUser u) (uTree u) n
      case mg of
        Nothing -> devwriteIO queue (Fail 404 Nothing)
        Just g  -> do
          devwriteIO queue (Item $ Name (uUser u) (uTree u) n g)
          evalLQL db core queue xs

evalFinalizer :: FH -> Device Reply -> Either SomeException () -> IO ()
evalFinalizer chan dev (Left e)  = do
  devwriteIO dev (encodeE e) `catch` ignore
  closeIO dev
  linfo Network $ printf "[fd: %s] session terminated with failure: %s" (show chan) (show e)
evalFinalizer chan dev (Right _)   = do
  closeIO dev
  linfo Network $ printf "[fd: %s] session terminated successfully" (show chan)

process :: (GraphBackend m) => m -> CoreServer -> Query -> IO Reply
process storage srv (Begin sig msg) =
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> return $ Fail 400 (Just "syntax error")
    Right stmts -> do
      (fh, dev) <- makeFD srv (sigUser sig)
      lnotice Network (printf "BEGIN %s %d" (show msg) fh)
      _         <- forkFinally (evalLQL storage srv dev stmts) (evalFinalizer fh dev)
      return $ Done fh
process _ srv (Fetch sig fh) = do
  let channel = (sigUser sig, fh)
  ldebug Network (printf "FETCH %d" fh)
  mdev <- selectFD srv channel
  case mdev of
    Nothing  -> return $ Fail 404 $ Just "no such channel"
    Just dev -> do
      blocks <- blkreadIO 32 dev
      case blocks of
        [] -> closeIO dev >> return (Last Nothing)
        _  -> let answer = foldr1 reduce blocks
              in when (isEOF answer) (closeIO dev) >> return answer
process _ srv (Close nowait sig fh) = do
  lnotice Network (printf "CLOSE %d" fh)
  closeFD srv nowait (sigUser sig, fh)
  return $ Last Nothing
