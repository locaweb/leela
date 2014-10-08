{-# LANGUAGE OverloadedStrings #-}
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

module Leela.Network.WarpServer
       ( WarpServer (logger)
       , warpServer
       , stopRouter
       , newWarpServer
       , warpLogServer
       ) where

import           Data.IORef
import           Data.Monoid ((<>))
import           System.ZMQ4
import           Leela.Logger
import           Data.Foldable (toList)
import           Leela.Helpers
import           Control.Monad
import           Leela.Data.LQL
import           Leela.Data.Time
import           Leela.Data.Graph
import           Leela.Data.Types
import qualified Leela.Data.L2Map as M
import           Control.Exception
import           Leela.HZMQ.Dealer
import           Leela.HZMQ.Router
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.QDevice
import           Leela.Data.Endpoint
import           Leela.Data.LQL.Comp
import           Leela.Storage.Graph
import qualified Leela.Storage.Passwd as P
import qualified Data.ByteString.Lazy as L
import           Leela.Storage.Passwd
import           Leela.Data.TimeSeries
import           Control.Concurrent.STM
import           Leela.Network.Protocol
import           Data.ByteString.Builder
import           Control.Parallel.Strategies

data WarpServer = WarpServer { logger :: Logger
                             , stat   :: IORef [(String, [Endpoint])]
                             , passwd :: IORef P.Passwd
                             , fdseq  :: Counter FH
                             , fdlist :: M.L2Map L.ByteString FH (TVar (Double, Time, Device Reply))
                             }

data Stream a = Chunk a
              | Error SomeException
              | EOF

ttl :: Double
ttl = 60

readPasswd :: WarpServer -> IO P.Passwd
readPasswd = readIORef . passwd

dumpStat :: WarpServer -> IO [(L.ByteString, L.ByteString)]
dumpStat core =
  liftM (concatMap dumpEntry) $ readIORef (stat core)
    where
      dumpEntry (k, [])     = [(showEndpoint k, "")]
      dumpEntry (k, [e])    = [(showEndpoint k, dumpEndpoint e)]
      dumpEntry (k, (e:es)) = (showEndpoint k, dumpEndpoint e) : dumpEntry (k, es)

      showEndpoint k = toLazyByteString $ string7 "endpoint/" <> string7 k

newWarpServer :: Logger -> IORef [(String, [Endpoint])] -> IORef P.Passwd -> IO WarpServer
newWarpServer syslog statdb secretdb = makeState
    where
      makeState =
        liftM2 (WarpServer syslog statdb secretdb) newCounter M.empty

makeFD :: WarpServer -> User -> IO (Time, FH, Device Reply)
makeFD srv (User u) = do
  dev  <- atomically $ do
    ctrl <- control
    open ctrl 4
  time <- snapshot
  fd   <- next (fdseq srv)
  val  <- newTVarIO (seconds time + ttl, time, dev)
  M.insert u fd val (fdlist srv)
  _    <- forkFinally (killer fd $ seconds time) (finalizer fd)
  return (time, fd, dev)
    where
      finalizer fd (Left _)     = warning (logger srv) (printf "gc: PURGE %d" fd) >> closeFD srv (User u, fd)
      finalizer fd (Right True) = warning (logger srv) (printf "gc: PURGE %d" fd) >> closeFD srv (User u, fd)
      finalizer _ _             = return ()

      killer fd time = do
        threadDelay (1000 * 1000)
        mv <- M.lookup u fd (fdlist srv)
        case mv of
          Nothing   -> return False
          Just mvar -> do
            (t0, _, _) <- readTVarIO mvar
            if (time >= t0)
              then return True
              else killer fd (time + 1)

withFD :: WarpServer -> (User, FH) -> (Maybe (Device Reply) -> IO b) -> IO b
withFD srv ((User u), fh) action = do
  mvalue <- M.lookup u fh (fdlist srv)
  case mvalue of
    Nothing    -> action Nothing
    Just value -> mask $ \restore -> do
      dev <- setTick value
      restore (action $ Just dev) `finally` (setTick value)
    where
      setTick shmem = do
        tnow <- fmap seconds snapshot
        atomically $ do
          (_, time, dev) <- readTVar shmem
          writeTVar shmem (tnow + ttl, time, dev)
          return dev

closeFD :: WarpServer -> (User, FH) -> IO ()
closeFD srv ((User u), fh) = do
  mvalue <- M.delete u fh (fdlist srv)
  case mvalue of
    Nothing    -> return ()
    Just value -> do
      (_, t0, dev) <- readTVarIO value
      t1           <- snapshot
      closeIO dev
      notice (logger srv) (printf "CLOSE %d [%s ms]" fh (showDouble $ milliseconds (diff t1 t0)))

navigate :: (GraphBackend m) => m -> Device Reply -> (Matcher, [(GUID -> Matcher)]) -> IO ()
navigate db queue (source, pipeline) = do
  srcpipe <- openIO queue 16
  forkSource srcpipe
  dstpipe <- foldM forkFilter srcpipe pipeline
  copy dstpipe asReply queue
    where
      two (_, b, c) = (c, b)

      asReply (Chunk (feed, path)) = Just (Item (List $ map (Path . (: path) . two) feed))
      asReply EOF                  = Nothing
      asReply (Error e)            = throw e

      runFilter srcpipe f dstpipe = do
        mg <- devreadIO srcpipe
        case mg of
          Nothing                      -> return ()
          Just (EOF, _)                -> return ()
          Just (Chunk (feed, path), _) -> do
            forM_ feed (\(_, b, c) ->
              query db (devwriteIO dstpipe . Chunk . (, (c, b) : path)) (f c))
            runFilter srcpipe f dstpipe
          Just (chunk, _)              -> devwriteIO dstpipe chunk

      forkSource dstpipe = do
        t <- forkFinally
               (query db (devwriteIO dstpipe . Chunk . (, [])) source)
               (either (devwriteIO dstpipe . Error) (const $ devwriteIO dstpipe EOF))
        addFinalizer queue (killThread t)

      forkFilter srcpipe f = do
        dstpipe <- openIO srcpipe 16
        t       <- forkFinally
          (runFilter srcpipe f dstpipe)
          (either (devwriteIO dstpipe . Error) (const $ devwriteIO dstpipe EOF))
        addFinalizer queue (killThread t)
        return dstpipe

evalLQL :: (GraphBackend m, AttrBackend m) => m -> WarpServer -> Device Reply -> [LQL] -> IO ()
evalLQL _ _ queue []         = devwriteIO queue Last
evalLQL db core queue (x:xs) = do
  case x of
    PathStmt q         ->
      navigate db queue q
    TAttrListStmt g a  ->
      enumTAttrs db (devwriteIO queue . Item . NAttrs g) g a
    KAttrListStmt g a  ->
      enumKAttrs db (devwriteIO queue . Item . NAttrs g) g a
    KAttrGetStmt g a _ ->
      getAttr db g a >>= devwriteIO queue . Item . KAttr g a
    TAttrGetStmt g a (Range t0 t1) pipeline -> do
      loadTAttr db pipeline (onTimeseries (devwriteIO queue . (Item . TAttr g a))) g a t0 t1
      devwriteIO queue (Item $ TAttr g a [])
    TAttrLastStmt guid attr -> do
      let toTAttr (g, a, t, v) = TAttr g a [(t, v)]
      scanLast db guid attr (\value ->
        case value of
          Just [] -> return ()
          Just xs -> devwriteIO queue (Item $ List (map toTAttr xs))
          Nothing -> devwriteIO queue (Fail 500 (Just "error reading from storage")))
    StatStmt          -> do
      state <- dumpStat core
      devwriteIO queue (Item $ Stat state)
    AlterStmt journal -> do
      names <- exec db (toList journal)
      mapM_ (\(u, t, k, n, g) -> devwriteIO queue (Item $ Name u t k n g)) names
    NameStmt _ guids  -> do
      names <- getName db (toList guids)
      forM_ names (\(u, t, k, n, g) ->
        devwriteIO queue (Item $ Name u t k n g))
    GUIDStmt user names  -> do
      guids <- getGUID db [(targetUser user, uTree user, k, n) | (k, n) <- toList names]
      forM_ guids (\(u, t, k, n, g) ->
        devwriteIO queue (Item $ Name u t k n g))
  evalLQL db core queue xs

evalFinalizer :: Logger -> Time -> FH -> Device Reply -> Either SomeException () -> IO ()
evalFinalizer syslog t0 chan dev (Left e)  = do
  t <- fmap (`diff` t0) snapshot
  warning syslog $ printf "FAILURE: %s %s [%s ms]" (show chan) (show e) (showDouble $ milliseconds t)
  devwriteIO dev (encodeE e) `catch` ignore
  closeIO dev
evalFinalizer syslog t0 chan dev (Right _)   = do
  t <- fmap (`diff` t0) snapshot
  notice syslog $ printf "SUCCESS: %s [%s ms]" (show chan) (showDouble $ milliseconds t)
  closeIO dev

onlyStat :: [LQL] -> Bool
onlyStat = and . map isStat

process :: (GraphBackend m, AttrBackend m) => m -> WarpServer -> Query -> IO Reply
process storage srv (Begin sig msg) =
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> return $ Fail 400 (Just "syntax error")
    Right stmts -> do
      (time, fh, dev) <- makeFD srv (sigUser sig)
      if (level (logger srv) >= NOTICE)
       then notice (logger srv) (printf "BEGIN %s %d" (lqlDescr stmts) fh)
       else info (logger srv) (printf "BEGIN %s %d" (show msg) fh)
      if (onlyStat stmts)
       then do
         result <- try $ evalLQL storage srv dev stmts
         evalFinalizer (logger srv) time fh dev result
       else do
        t <- forkFinally (evalLQL storage srv dev stmts) (evalFinalizer (logger srv) time fh dev)
        addFinalizer dev (killThread t)
      return $ Done fh
process _ srv (Fetch sig fh)        = do
  let channel = (sigUser sig, fh)
  notice (logger srv) (printf "FETCH %d" fh)
  withFD srv channel $ \mdev ->
    case mdev of
      Nothing  -> return $ Fail 404 $ Just "no such channel"
      Just dev -> devreadIO dev >>= \mreply ->
        case mreply of
          Just (reply, hasMore) -> do
            when (isEOF reply || not hasMore) (closeFD srv (sigUser sig, fh))
            return reply
          Nothing               -> do
            closeFD srv (sigUser sig, fh)
            return Last
process _ srv (Close _ sig fh)      = do
  closeFD srv (sigUser sig, fh)
  return Last

strictEncode :: Reply -> [L.ByteString]
strictEncode = withStrategy (evalList rdeepseq) . encode

warpServer :: (GraphBackend m, AttrBackend m) => WarpServer -> NominalDiffTime -> Endpoint -> Context -> m -> IO RouterFH
warpServer core ttl addr ctx storage = startRouter (logger core) addr ctx (worker storage core ttl)
    where
      worker db core ttl = Worker f (evaluate . strictEncode . encodeE)
        where
          f msg = do
            time     <- now
            secretdb <- readPasswd core
            case (decode (time, ttl) (readSecret secretdb) msg) of
              Left err -> evaluate $ strictEncode err
              Right q  -> process db core q >>= evaluate . strictEncode

warpLogServer :: (GraphBackend m, AttrBackend m) => ClientFH Push -> m -> LogBackend m
warpLogServer _ db =
  logBackend handleAttrEvent handleGraphEvent db
    where
      handleAttrEvent _ = return ()

      handleGraphEvent _ = return ()
