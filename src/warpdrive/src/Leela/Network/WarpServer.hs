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
import           System.IO.Unsafe
import           Control.Exception
import           Leela.HZMQ.Dealer
import           Leela.HZMQ.Router
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.QDevice
import           Leela.Data.Timeout
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

data WarpServer = WarpServer { logger   :: Logger
                             , stat     :: IORef [(String, [Endpoint])]
                             , passwd   :: IORef P.Passwd
                             , fdseq    :: Counter FH
                             , tManager :: Manager
                             , fdlist   :: M.L2Map L.ByteString FH (TVar (Handle, Time, QDevice Reply))
                             }

data Stream a = Chunk a
              | Error SomeException
              | EOF

useTimeout :: Int
useTimeout = 30 * 1000 * 1000

serverLimit :: Int
serverLimit = unsafePerformIO (liftM (* 100) getNumCapabilities)

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
        liftM3 (WarpServer syslog statdb secretdb) newCounter timeoutManager M.empty
    
makeFD :: WarpServer -> User -> (Time -> Handle -> FH -> QDevice Reply -> IO ()) -> IO ()
makeFD srv (User u) cc = do
  fd   <- next (fdseq srv)
  mth  <- open (tManager srv) (<= serverLimit) useTimeout (closeFDTimeout srv (User u, fd))
  case mth of
    Nothing           -> warning (logger srv) (printf "REJECT %d : [server-limit:%d]" fd serverLimit)
    Just (active, th) -> void $ forkIO $ do
      notice (logger srv) (printf "ACCEPT %d : %d" fd active)
      dev  <- qnew 4
      time <- snapshot
      val  <- newTVarIO (th, time, dev)
      M.insert u fd val (fdlist srv)
      cc time th fd dev

withFD :: WarpServer -> (User, FH) -> (Maybe (QDevice Reply) -> IO b) -> IO b
withFD srv ((User u), fh) action = do
  mvalue <- M.lookup u fh (fdlist srv)
  case mvalue of
    Nothing   -> action Nothing
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

evalLQL :: (GraphBackend m, AttrBackend m) => m -> WarpServer -> Handle -> QDevice Reply -> [LQL] -> IO ()
evalLQL _ _ thandle queue []         = touchQWrite thandle queue Last
evalLQL db core thandle queue (x:xs) = do
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
    AlterStmt journal -> do
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
  evalLQL db core thandle queue xs

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

process :: (GraphBackend m, AttrBackend m) => m -> WarpServer -> Query -> (Reply -> IO ()) -> IO ()
process storage srv (Begin sig msg) flush =
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> flush $ Fail 400 (Just "syntax error")
    Right stmts -> makeFD srv (sigUser sig) $ \time thandle fh dev -> do
      if (level (logger srv) >= NOTICE)
        then notice (logger srv) (printf "BEGIN %s %d" (lqlDescr stmts) fh)
        else info (logger srv) (printf "BEGIN %s %d" (show msg) fh)
      flush $ Done fh
      result <- try $ evalLQL storage srv thandle dev stmts
      evalFinalizer (logger srv) time fh dev result
process _ srv (Fetch sig fh) flush = do
  let channel = (sigUser sig, fh)
  notice (logger srv) (printf "FETCH %d" fh)
  withFD srv channel $ \mdev ->
    case mdev of
      Nothing  -> flush $ Fail 404 $ Just "no such channel"
      Just dev -> qTryRead dev >>= \msg ->
        case msg of
          (Nothing, True)  -> do
            void $ forkIO (qread dev >>= \msg ->
              case msg of
                Just r   -> do
                  when (isEOF r) (closeFD srv (sigUser sig, fh))
                  flush r
                Nothing  -> do
                  closeFD srv (sigUser sig, fh)
                  flush Last)
          (Nothing, False) -> do
            closeFD srv (sigUser sig, fh)
            flush Last
          (Just r, hasMore)   -> do
            when (not hasMore) (closeFD srv (sigUser sig, fh))
            flush r
process _ srv (Close _ sig fh) flush = do
  closeFD srv (sigUser sig, fh)
  flush Last

warpServer :: (GraphBackend m, AttrBackend m) => WarpServer -> NominalDiffTime -> Endpoint -> Context -> m -> IO RouterFH
warpServer core ttl addr ctx storage = startRouter (logger core) addr ctx (worker storage core ttl)
    where
      worker db core ttl = Worker f (return . encode . encodeE)
        where
          f msg flush = do
            time     <- now
            secretdb <- readPasswd core
            case (decode (time, ttl) (readSecret secretdb) msg) of
              Left e@(Fail c m) -> do
                notice (logger core) (printf "FAIL %d %s" c (maybe "" id m))
                flush (encode e)
              Left e            -> flush (encode e)
              Right q           -> process db core q (flush . encode)

warpLogServer :: (GraphBackend m, AttrBackend m) => ClientFH Push -> m -> LogBackend m
warpLogServer _ db =
  logBackend handleAttrEvent handleGraphEvent db
    where
      handleAttrEvent _ = return ()

      handleGraphEvent _ = return ()
