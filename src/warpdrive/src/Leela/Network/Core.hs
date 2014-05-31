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
       ( CoreServer (logger)
       , newCore
       , process
       , readPasswd
       ) where

import qualified Data.Map as M
import           Data.IORef
import           Leela.Logger
import           Leela.Helpers
import           Control.Monad
import           Leela.Data.LQL
import qualified Data.ByteString as B
import           Leela.Data.Graph
import           Leela.Data.Types
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.QDevice
import           Leela.Data.Endpoint
import           Leela.Data.LQL.Comp
import           Leela.Storage.Graph
import qualified Leela.Storage.Passwd as P
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.UTF8 (fromString)
import           Control.Concurrent.STM
import           Leela.Network.Protocol
import           Leela.Storage.KeyValue

data CoreServer = CoreServer { logger :: Logger
                             , stat   :: IORef [(String, [Endpoint])]
                             , passwd :: IORef P.Passwd
                             , fdseq  :: TVar FH
                             , fdlist :: TVar (M.Map (B.ByteString, FH) (Int, Device Reply))
                             }

ttl :: Int
ttl = 300

readPasswd :: CoreServer -> IO P.Passwd
readPasswd = readIORef . passwd

dumpStat :: CoreServer -> IO [(B.ByteString, B.ByteString)]
dumpStat core = do
  liftM (concatMap dumpEntry) $ readIORef (stat core)
    where
      dumpEntry (k, [])     = [(fromString $ "endpoint/" ++ k, "")]
      dumpEntry (k, [e])    = [(fromString $ "endpoint/" ++ k, toStrict $ dumpEndpoint e)]
      dumpEntry (k, (e:es)) = (fromString $ "endpoint/" ++ k, toStrict $ dumpEndpoint e) : dumpEntry (k, es)

newCore :: Logger -> IORef [(String, [Endpoint])] -> IORef P.Passwd -> IO CoreServer
newCore syslog statdb secretdb = do
  state <- makeState
  _     <- forkIO (forever (sleep 1 >> rungc syslog (fdlist state)))
  return state
    where
      makeState = atomically $
        liftM2 (CoreServer syslog statdb secretdb) (newTVar 0) (newTVar M.empty)

rungc :: (Ord k, Show k) => Logger -> TVar (M.Map k (Int, Device a)) -> IO ()
rungc syslog tvar = atomically kill >>= mapM_ burry
    where
      partition acc [] = acc
      partition (a, b) ((k, (tick, dev)):xs)
        | tick == 0    = partition ((k, dev) : a, b) xs
        | otherwise    = partition (a, (k, (tick - 1, dev)) : b) xs

      kill = do
        (dead, alive) <- fmap (partition ([], []) . M.toList) (readTVar tvar)
        writeTVar tvar (M.fromList alive)
        return dead

      burry (k, dev) = do
        warning syslog $ printf "closing/purging unused channel: %s" (show k)
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
  debug (logger srv) (printf "closing fd %s" (show k))
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
            forM_ feed (\(_, b, c) ->
              query db (devwriteIO dstpipe . Chunk . (, (c, b) : path)) (f $ c))
            runFilter srcpipe f dstpipe
          Just chunk                -> devwriteIO dstpipe chunk

      forkSource dstpipe = void $ forkFinally
        (query db (devwriteIO dstpipe . Chunk . (, [])) source)
        (either (devwriteIO dstpipe . Error) (const $ devwriteIO dstpipe EOF))

      forkFilter srcpipe f = do
        dstpipe <- openIO srcpipe 16
        _       <- forkFinally
          (runFilter srcpipe f dstpipe)
          (either (devwriteIO dstpipe . Error) (const $ devwriteIO dstpipe EOF))
        return dstpipe

      forkFilters srcpipe []     = return srcpipe
      forkFilters srcpipe (f:fs) = do
        dstpipe <- forkFilter srcpipe f
        forkFilters dstpipe fs

evalLQL :: (KeyValue cache, GraphBackend m, AttrBackend m) => cache -> m -> CoreServer -> Device Reply -> [LQL] -> IO ()
evalLQL _ _ _ queue []             = devwriteIO queue Last
evalLQL cache db core queue (x:xs) = do
  case x of
    PathStmt q         ->
      navigate db queue q
    TAttrListStmt g a  ->
      enumTAttrs db (devwriteIO queue . Item . NAttrs g) g a
    KAttrListStmt g a  ->
      enumKAttrs db (devwriteIO queue . Item . NAttrs g) g a
    KAttrGetStmt g a _ ->
      getAttr db g a >>= devwriteIO queue . Item . KAttr g a
    TAttrGetStmt g a (Range t0 t1) _ -> do
      loadTAttr db (devwriteIO queue . either (flip Fail Nothing) (Item . TAttr g a)) g a t0 t1
      devwriteIO queue (Item $ TAttr g a [])
    StatStmt          -> do
      state <- dumpStat core
      devwriteIO queue (Item $ Stat state)
    AlterStmt journal -> do
      names <- exec db journal
      mapM_ (\(u, t, k, n, g) -> devwriteIO queue (Item $ Name u t k n g)) names
    NameStmt _ guids  -> do
      names <- getName db guids
      forM_ names (\(u, t, k, n, g) ->
        devwriteIO queue (Item $ Name u t k n g))
    GUIDStmt user names  -> do
      guids <- getGUID db [(targetUser user, uTree user, k, n) | (k, n) <- names]
      forM_ guids (\(u, t, k, n, g) ->
        devwriteIO queue (Item $ Name u t k n g))
  evalLQL cache db core queue xs

evalFinalizer :: Logger -> FH -> Device Reply -> Either SomeException () -> IO ()
evalFinalizer syslog chan dev (Left e)  = do
  devwriteIO dev (encodeE e) `catch` ignore
  closeIO dev
  notice syslog $ printf "[fd: %s] session terminated with failure: %s" (show chan) (show e)
evalFinalizer syslog chan dev (Right _)   = do
  closeIO dev
  info syslog $ printf "[fd: %s] session terminated successfully" (show chan)

process :: (KeyValue cache, GraphBackend m, AttrBackend m) => cache -> m -> CoreServer -> Query -> IO Reply
process cache storage srv (Begin sig msg) =
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> return $ Fail 400 (Just "syntax error")
    Right stmts -> do
      (fh, dev) <- makeFD srv (sigUser sig)
      if (level (logger srv) >= NOTICE)
        then notice (logger srv) (printf "BEGIN ... %d" fh)
        else info (logger srv) (printf "BEGIN %s %d" (show msg) fh)
      _         <- forkFinally (evalLQL cache storage srv dev stmts) (evalFinalizer (logger srv) fh dev)
      return $ Done fh
process _ _ srv (Fetch sig fh) = do
  let channel = (sigUser sig, fh)
  notice (logger srv) (printf "FETCH %d" fh)
  mdev <- selectFD srv channel
  case mdev of
    Nothing  -> return $ Fail 404 $ Just "no such channel"
    Just dev -> do
      mmsg <- devreadIO dev
      case mmsg of
        Nothing  -> closeIO dev >> return Last
        Just msg -> return msg
process _ _ srv (Close nowait sig fh) = do
  notice (logger srv) (printf "CLOSE %d" fh)
  closeFD srv nowait (sigUser sig, fh)
  return Last
