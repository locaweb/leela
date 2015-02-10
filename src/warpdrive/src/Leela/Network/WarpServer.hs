{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
       ) where

import           Data.IORef
import           Data.Monoid ((<>))
import           System.ZMQ4
import           Leela.Logger
import           Data.Foldable (toList)
import           Control.Monad
import qualified Data.Sequence as Sq
import           Leela.Data.LQL
import           Leela.Data.Time
import           Leela.HZMQ.Pipe
import           Leela.Data.Graph
import           Leela.Data.Types
import qualified Leela.Data.L2Map as M
import           System.IO.Unsafe
import           Control.Exception
import           Leela.HZMQ.Router
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.QDevice
import           Leela.Data.Timeout
import           Leela.Data.Endpoint
import           Leela.Storage.Graph
import           Leela.Data.LQL.Read
import           Leela.Data.LQL.Show
import qualified Leela.Storage.Passwd as P
import qualified Data.ByteString.Lazy as L
import           Leela.Storage.Passwd
import           Data.ByteString.Char8 (unpack)
import           Leela.Data.TimeSeries
import           Control.Concurrent.STM
import           Leela.Network.Protocol
import           Data.ByteString.Builder
import           Leela.Network.GrepProtocol
import           Data.Double.Conversion.ByteString

data WarpServer = WarpServer { logger     :: Logger
                             , stat       :: IORef [(String, [Endpoint])]
                             , passwd     :: IORef P.Passwd
                             , warpGrep   :: Pipe Push
                             , fdseq      :: Counter FH
                             , tManager   :: TimeoutManager
                             , fdlist     :: M.L2Map L.ByteString FH (TVar (Handle, Time, QDevice Reply))
                             , checkpoint :: IORef (Time, FH)
                             }

data Stream a = Chunk a
              | Error SomeException
              | EOF

showDouble :: Double -> String
showDouble = unpack . toShortest

serverLimit :: Int
serverLimit = 128 * (unsafePerformIO getNumCapabilities)

useTimeoutInMs :: Num a => a
useTimeoutInMs = 90 * 1000

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

newWarpServer :: Logger -> Pipe Push -> IORef [(String, [Endpoint])] -> IORef P.Passwd -> IO WarpServer
newWarpServer syslog pipe statdb secretdb = makeState
    where
      makeState = do
        time <- now
        liftM4 (WarpServer syslog statdb secretdb pipe)
          newCounter timeoutManager M.empty (newIORef (time, 0))

makeCheckpoint :: WarpServer -> Time -> FH -> IO ()
makeCheckpoint srv t1 fh1 = do
  new <- atomicModifyIORef' (checkpoint srv) (uncurry modify)
  when new (notice (logger srv) (printf "creating checkpoint: %f:%d" (seconds t1) fh1))
    where
      elapsed t1 t0 = milliseconds $ diff t1 t0

      modify t0 fh0 =
        if (elapsed t1 t0 > useTimeoutInMs)
          then ((t1, fh1), True)
          else ((t0, fh0), False)

makeFD :: WarpServer -> User -> (Time -> Handle -> FH -> QDevice Reply -> IO ()) -> IO ()
makeFD srv (User u) cc = do
  fd           <- next (fdseq srv)
  (active, th) <- open (tManager srv) (useTimeoutInMs * 1000) (closeFDTimeout srv (User u, fd))
  if (active > serverLimit)
    then do
      purge th
      warning (logger srv) (printf "REJECT %d" active)
    else do
      notice (logger srv) (printf "ACCEPT %d : %d" fd active)
      dev  <- qnew 16
      time <- snapshot
      val  <- newTVarIO (th, time, dev)
      M.insert u fd val (fdlist srv)
      makeCheckpoint srv time fd
      cc time th fd dev

withFD :: WarpServer -> (User, FH) -> (Maybe (QDevice Reply) -> IO ()) -> IO ()
withFD srv ((User u), fh) action = do
  mvalue <- M.lookup u fh (fdlist srv)
  case mvalue of
    Nothing   -> do
      (_, fh0) <- readIORef (checkpoint srv)
      when (fh >= fh0) (action Nothing)
    Just tvar -> do
      (th, _, dev) <- readTVarIO tvar
      touch th
      action $ Just dev

closeFDTimeout :: WarpServer -> (User, FH) -> IO ()
closeFDTimeout srv (u, fh) = do
  warning (logger srv) (printf "TIMEOUT %d" fh)
  closeFD srv (u, fh)
  
closeFD :: WarpServer -> (User, FH) -> IO ()
closeFD srv (User u, fh) = do
  mvalue <- M.delete u fh (fdlist srv)
  case mvalue of
    Nothing    -> return ()
    Just value -> do
      (th, t0, dev) <- readTVarIO value
      t1            <- snapshot
      purge th
      qclose dev
      notice (logger srv) (printf "CLOSE %d [%s ms]" fh (showDouble $ milliseconds (diff t1 t0)))

touchQWrite :: Handle -> QDevice a -> a -> IO ()
touchQWrite th q a = touch th >> qwrite q a

navigate :: (GraphBackend m) => m -> Handle -> QDevice Reply -> (Matcher, [(Bool, GUID -> Matcher)]) -> IO ()
navigate db thandle queue (source, pipeline) = do
  srcpipe <- qlink 16 queue
  forkSource srcpipe
  dstpipe <- foldM forkFilter srcpipe pipeline
  copy asReply dstpipe queue
    where
      two (_, b, c) = (c, b)

      asReply (Chunk (feed, path))
        | null feed && null path    = (True, Nothing)
        | null feed                 = (True, Just $ Item (Path path))
        | otherwise                 = (True, Just $ Item (List $ map (Path . (: path) . two) feed))
      asReply EOF                   = (False, Nothing)
      asReply (Error e)             = throw e

      selectIf nilOk io xs
        | nilOk || (not $ null xs) = io xs
        | otherwise                = return ()

      runFilter nilOk srcpipe f dstpipe = do
        mg <- qread srcpipe
        case mg of
          Nothing                   -> touch thandle
          Just (Chunk ([], path))   -> do
            unless (null path) (touchQWrite thandle queue $ Item $ Path path)
            runFilter nilOk srcpipe f dstpipe
          Just (Chunk (feed, path)) -> do
            forM_ feed (\(_, b, c) ->
              query db (selectIf nilOk $ touchQWrite thandle dstpipe . Chunk . (, (c, b) : path)) (f c))
            runFilter nilOk srcpipe f dstpipe
          Just chunk                -> touchQWrite thandle dstpipe chunk

      forkSource dstpipe =
        forkFinally
          (query db (selectIf False $ touchQWrite thandle dstpipe . Chunk . (, [])) source)
          (onTerm dstpipe)
          where
            onTerm dstpipe (Left e) = qTryWrite dstpipe (Error e)
            onTerm dstpipe _        = touchQWrite thandle dstpipe EOF

      forkFilter srcpipe (nilOk, f) = do
        dstpipe <- qlink 16 queue
        forkFinally
          (runFilter nilOk srcpipe f dstpipe)
          (onTerm dstpipe)
        return dstpipe
          where
            onTerm dstpipe (Left e) = qTryWrite dstpipe (Error e)
            onTerm _ _              = return ()

evalLQL :: (GraphBackend m, AttrBackend m) => Context -> m -> WarpServer -> Handle -> QDevice Reply -> [LQL] -> IO ()
evalLQL _ _ _ thandle queue []           = touchQWrite thandle queue Last
evalLQL ctx db core thandle queue (x:xs) = do
  case x of
    PathStmt q         ->
      navigate db thandle queue q
    TAttrListStmt g a  ->
      enumTAttrs db (touchQWrite thandle queue . Item . NAttrs g) g a
    KAttrListStmt g a  ->
      enumKAttrs db (touchQWrite thandle queue . Item . NAttrs g) g a
    KAttrGetStmt g a _ ->
      getAttr db g a >>= touchQWrite thandle queue . Item . KAttr g a
    TAttrGetStmt g a (Range t0 t1) pipeline -> do
      loadTAttr db pipeline (onTimeseries (touchQWrite thandle queue . (Item . TAttr g a))) g a t0 t1
      touchQWrite thandle queue (Item $ TAttr g a [])
    TAttrLastStmt guid attr -> do
      let toTAttr (g, a, t, v) = TAttr g a [(t, v)]
      scanLast db guid attr (\value ->
        case value of
          Just [] -> return ()
          Just xs -> touchQWrite thandle queue (Item $ List (map toTAttr xs))
          Nothing -> touchQWrite thandle queue (Fail 500 (Just "error reading from storage")))
    StatStmt          -> do
      state <- dumpStat core
      touchQWrite thandle queue (Item $ Stat state)
    AlterStmt journal
      | Sq.length journal > 1000 ->
        touchQWrite thandle queue (Fail 413 (Just $ printf "too many write requests [%d > 1000]" (Sq.length journal)))
      | otherwise                -> do
        names <- exec db (toList journal)
        mapM_ (\(u, t, k, n, g) -> touchQWrite thandle queue (Item $ Name u t k n g)) names
    NameStmt _ guids  -> do
      names <- getName db (toList guids)
      forM_ names (\(u, t, k, n, g) ->
        touchQWrite thandle queue (Item $ Name u t k n g))
    GUIDStmt user names  -> do
      guids <- getGUID db [(targetUser user, uTree user, k, n) | (k, n) <- toList names]
      forM_ guids (\(u, t, k, n, g) ->
        touchQWrite thandle queue (Item $ Name u t k n g))
    GrepStmt u query     -> do
      t <- now
      broadcast ctx 1000 (warpGrep core) [encodeEventMessage $ ControlMsg t query]
      touchQWrite thandle queue (Item $ Name (uUser u) (uTree u) (Kind "lql/grep") (Node $ renderGrep query) (grepID query))
  evalLQL ctx db core thandle queue xs

evalFinalizer :: Logger -> Time -> FH -> QDevice Reply -> Either SomeException () -> IO ()
evalFinalizer syslog t0 chan dev (Left e)  = do
  t <- liftM (`diff` t0) snapshot
  warning syslog $ printf "FAILURE: %s %s [%s ms]" (show chan) (show e) (showDouble $ milliseconds t)
  qTryWrite dev (encodeE e)
  qclose dev
evalFinalizer syslog t0 chan dev (Right _)   = do
  t <- liftM (`diff` t0) snapshot
  notice syslog $ printf "SUCCESS: %s [%s ms]" (show chan) (showDouble $ milliseconds t)
  qclose dev

process :: (GraphBackend m, AttrBackend m) => Context -> m -> WarpServer -> Query -> (Reply -> IO ()) -> IO ()
process ctx storage srv (Begin sig msg) flush = void $ forkIO $ yield >>
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> flush $ Fail 400 (Just "syntax error")
    Right stmts -> makeFD srv (sigUser sig) (\time thandle fh dev -> do
      if (level (logger srv) >= NOTICE)
        then notice (logger srv) (printf "BEGIN %s %d" (lqlDescr stmts) fh)
        else info (logger srv) (printf "BEGIN %s %d" (show msg) fh)
      flush $ Done fh
      result <- try $ evalLQL ctx storage srv thandle dev stmts
      evalFinalizer (logger srv) time fh dev result)
process _ _ srv (Fetch sig fh) flush          = void $ forkIO $ do
  let channel = (sigUser sig, fh)
  notice (logger srv) (printf "FETCH %d" fh)
  withFD srv channel $ \mdev ->
    case mdev of
      Nothing  -> flush $ Fail 404 $ Just "no such channel"
      Just dev -> qread dev >>= \msg ->
        case msg of
          Just r   -> do
            when (isEOF r) (closeFD srv (sigUser sig, fh))
            flush r
          Nothing  -> do
            closeFD srv (sigUser sig, fh)
            flush Last
process _ _ srv (Close _ sig fh) flush        = do
  closeFD srv (sigUser sig, fh)
  flush Last

warpServer :: (GraphBackend m, AttrBackend m) => WarpServer -> NominalDiffTime -> TimeCache -> Endpoint -> Context -> m -> IO RouterFH
warpServer core ttl tcache addr ctx storage =
  startRouter (logger core) addr ctx (worker (logBackend storage handleGraphEvent handleAttrEvent) core ttl)
    where
      worker db core ttl = Worker f (return . encode . encodeE)
        where
          f msg flush = do
            time     <- readCache tcache
            secretdb <- readPasswd core
            case (decode (time, ttl) (readSecret secretdb) msg) of
              Left e@(Fail c m) -> do
                notice (logger core) (printf "FAIL %d %s" c (maybe "" id m))
                flush (encode e)
              Left e            -> flush (encode e)
              Right q           -> process ctx db core q (flush . encode)

      handleAttrEvent :: [AttrEvent] -> IO ()
      handleAttrEvent e = do
        t  <- readCache tcache
        ok <- push (warpGrep core) [encodeEventMessage $ AttrDataMsg t e]
        unless ok $ debug (logger core) (printf "warpserver: dropping attr event [queue full]")

      handleGraphEvent :: [GraphEvent] -> IO ()
      handleGraphEvent e = do
        t  <- readCache tcache
        ok <- push (warpGrep core) [encodeEventMessage $ GraphDataMsg t e]
        unless ok $ debug (logger core) (printf "warpserver: dropping graph event [queue full]")
