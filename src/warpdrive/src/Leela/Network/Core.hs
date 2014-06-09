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

import           Data.IORef
import           Leela.Logger
import           Leela.Helpers
import           Control.Monad
import           Leela.Data.LQL
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Graph
import           Leela.Data.Types
import qualified Leela.Data.L2Map as M
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Counter
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
                             , tick   :: Counter Tick
                             , fdseq  :: Counter FH
                             , fdlist :: M.L2Map B.ByteString FH (TVar (Tick, TimeSpec, Device Reply))
                             }

ttl :: Tick
ttl = 60

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
  _     <- forkSupervised_ syslog "rungc" (sleep 1 >> rungc syslog state)
  return state
    where
      makeState =
        liftM3 (CoreServer syslog statdb secretdb) newCounter newCounter M.empty

rungc :: Logger -> CoreServer -> IO ()
rungc syslog srv = do
  at <- next (tick srv)
  kill at >>= mapM_ burry
    where
      partition _ [] acc               = return acc
      partition curr ((k, v) : xs) acc = do
        (at, _, _) <- atomically $ readTVar v
        case at of
          0                                     -> partition curr xs acc
          _
            | (max at curr - min at curr) > ttl -> partition curr xs (k : acc)
            | otherwise                         -> partition curr xs acc

      kill at = do
        dead <- M.toList (fdlist srv) (partition at) []
        return dead

      burry (u, fh) = do
        let k = (User u, fh)
        warning syslog $ printf "PURGE %s" (show k)
        void $ closeFD srv k

makeFD :: CoreServer -> User -> IO (FH, Device Reply)
makeFD srv (User u) = do
  dev  <- atomically $ do
    ctrl <- control
    open ctrl 512
  time <- snapshot
  fd   <- next (fdseq srv)
  at   <- peek (tick srv)
  val  <- time `seq` newTVarIO (at, time, dev)
  M.insert u fd val (fdlist srv)
  return (fd, dev)

withFD :: CoreServer -> (User, FH) -> (Maybe (Device Reply) -> IO b) -> IO b
withFD srv ((User u), fh) action = do
  mvalue <- M.lookup u fh (fdlist srv)
  case mvalue of
    Nothing    -> action Nothing
    Just value -> mask $ \restore -> do
      dev <- setTick value (Just 0)
      res <- restore (action $ Just dev) `onException` (void $ setTick value Nothing)
      _   <- setTick value Nothing
      return res
    where
      setTick shmem mvalue = do
        at <- fmap (\v -> maybe v id mvalue) (peek (tick srv))
        atomically $ do
          (_, time, dev) <- readTVar shmem
          writeTVar shmem (at, time, dev)
          return dev

closeFD :: CoreServer -> (User, FH) -> IO Double
closeFD srv ((User u), fh) = do
  mvalue <- M.delete u fh (fdlist srv)
  case mvalue of
    Nothing    -> return 0
    Just value -> do
      (_, t0, dev) <- atomically $ readTVar value
      t1           <- snapshot
      closeIO dev
      return (elapsed t1 t0)

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
  notice syslog $ printf "FAILURE: %s %s" (show chan) (show e)
evalFinalizer syslog chan dev (Right _)   = do
  closeIO dev
  notice syslog $ printf "SUCCESS: %s" (show chan)

process :: (KeyValue cache, GraphBackend m, AttrBackend m) => cache -> m -> CoreServer -> Query -> IO Reply
process cache storage srv (Begin sig msg) =
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> return $ Fail 400 (Just "syntax error")
    Right stmts -> do
      (fh, dev) <- makeFD srv (sigUser sig)
      if (level (logger srv) >= NOTICE)
        then notice (logger srv) (printf "BEGIN %s %d" (lqlDescr stmts) fh)
        else info (logger srv) (printf "BEGIN %s %d" (show msg) fh)
      _         <- forkFinally (evalLQL cache storage srv dev stmts) (evalFinalizer (logger srv) fh dev)
      return $ Done fh
process _ _ srv (Fetch sig fh) = do
  let channel = (sigUser sig, fh)
  notice (logger srv) (printf "FETCH %d" fh)
  withFD srv channel $ \mdev -> do
    case mdev of
      Nothing  -> return $ Fail 404 $ Just "no such channel"
      Just dev -> do
        mmsg <- devreadIO dev
        case mmsg of
          Nothing  -> return Last
          Just msg -> return msg
process _ _ srv (Close _ sig fh) = do
  t <- closeFD srv (sigUser sig, fh)
  notice (logger srv) (printf "CLOSE %d [%f ms]" fh (t / 10000000))
  return Last
