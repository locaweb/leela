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

module Leela.Network.WarpGrepServer
       ( WarpGrepServer ()
       , stopPipe
       , socketAdapter
       , warpGrepServer
       , newWarpGrepServer
       , registerOrRefresh
       ) where

import qualified Data.Map as M
import           Data.Maybe
import           System.ZMQ4 (Pull, Context, Sender, Socket)
import           Leela.Logger
import           Control.Monad
import           Data.Serialize
import           Leela.Data.LQL
import qualified Data.ByteString as B
import           Leela.HZMQ.Pipe
import           Leela.Data.Types
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Timeout
import           Control.Applicative
import           Leela.HZMQ.ZHelpers
import           Leela.Data.EventTree
import           Control.Concurrent.STM
import           Leela.Network.Protocol (encodeGEvent, encodeAEvent)

data Connection = Connection { connCount   :: TVar Int
                             , connEvent   :: Either (Distribution [GraphEvent] [GraphEvent]) (Distribution [AttrEvent] [AttrEvent])
                             , connWaitT   :: MVar ()
                             , connThread  :: MVar (Handle, ThreadId)
                             }

data EventTree = EventTree { tip   :: Distribution [B.ByteString] [Either GraphEvent AttrEvent]
                           , aTree :: Distribution [Either GraphEvent AttrEvent] [AttrEvent]
                           , gTree :: Distribution [Either GraphEvent AttrEvent] [GraphEvent]
                           }

data WarpGrepServer = WarpGrepServer { wgsLogger            :: Logger
                                     , wgsStdout            :: GUID -> Either GraphEvent AttrEvent -> IO ()
                                     , wgsEventTree         :: EventTree
                                     , wgsIOThreads         :: TVar (M.Map GUID Connection)
                                     , wgsTimeoutManager    :: TimeoutManager
                                     }

defaultTimeout :: TimeoutInUs
defaultTimeout = 900 * 1000 * 1000

socketAdapter :: (Sender a) => Socket a -> GUID -> Either GraphEvent AttrEvent -> IO ()
socketAdapter fh guid = sendAll' fh . either (encodeGEvent guid) (encodeAEvent guid)

newWarpGrepServer :: Logger -> (GUID -> Either GraphEvent AttrEvent -> IO ()) -> IO WarpGrepServer
newWarpGrepServer logger writer = WarpGrepServer logger writer <$> eventTree <*> newTVarIO M.empty <*> timeoutManager

selectList :: [a] -> Maybe [a]
selectList [] = Nothing
selectList xs = Just xs

rootDecode :: IO (Distribution [B.ByteString] [Either GraphEvent AttrEvent])
rootDecode = makeStartDistribution selector
  where
    selector ("g" : _ : events) = selectList $ mapMaybe (either (const Nothing) (Just . Left) . decode) events
    selector ("a" : _ : events) = selectList $ mapMaybe (either (const Nothing) (Just . Right) . decode) events
    selector _                  = Nothing

graphBranch :: IO (Distribution [Either GraphEvent AttrEvent] [GraphEvent])
graphBranch = makeStartDistribution (selectList . mapMaybe (either Just (const Nothing)))

attrBranch :: IO (Distribution [Either GraphEvent AttrEvent] [AttrEvent])
attrBranch = makeStartDistribution (selectList . mapMaybe (either (const Nothing) Just))

feed :: WarpGrepServer -> [B.ByteString] -> IO ()
feed srv = publish (tip $ wgsEventTree srv)

pipeFeed :: WarpGrepServer -> Pipe Pull -> IO ()
pipeFeed srv pipe = mapM_ (feed srv) =<< pull pipe

eventTree :: IO EventTree
eventTree = do
  root    <- rootDecode   
  abranch <- attrBranch
  gbranch <- graphBranch
  attach root abranch
  attach root gbranch
  return $ EventTree root abranch gbranch

registerConn :: WarpGrepServer -> GUID -> Connection -> IO Connection
registerConn srv guid newConn = atomically $ do
  m <- readTVar (wgsIOThreads srv)
  case (M.lookup guid m) of
    Just conn -> return conn
    Nothing   -> do
      writeTVar (wgsIOThreads srv) (M.insert guid newConn m)
      return newConn

registerOrRefresh :: WarpGrepServer -> Grep -> IO ()
registerOrRefresh srv selector = do
  notice (wgsLogger srv) (printf "registering new query: %s" (show guid))
  ctrl     <- newTVarIO 0
  cookie   <- newEmptyMVar
  ioThread <- newEmptyMVar
  curConn  <- case (grep selector) of
                Right fun -> do
                  dist <- makeDistribution (selectList . filter fun)
                  registerConn srv guid (Connection ctrl (Right dist) cookie ioThread)
                Left fun  -> do 
                  dist <- makeDistribution (selectList . filter fun)
                  registerConn srv guid (Connection ctrl (Left dist) cookie ioThread)
  canStart <- atomically $ do
    active <- readTVar (connCount curConn)
    writeTVar (connCount curConn) (active + 1)
    return $ active == 0
  when canStart $ do
    notice (wgsLogger srv) (printf "starting distribution point/iothread: %s" (show guid))
    (_, cookie) <- open (wgsTimeoutManager srv) defaultTimeout (unregister srv guid)
    case (connEvent curConn) of
      Left dist  -> do
        startDistribution dist
        attach (gTree $ wgsEventTree srv) dist
        pid <- forkIOThread srv guid curConn
        putMVar (connThread curConn) (cookie, pid)
      Right dist -> do
        startDistribution dist
        attach (aTree $ wgsEventTree srv) dist
        pid <- forkIOThread srv guid curConn
        putMVar (connThread curConn) (cookie, pid)
  (cookie, _) <- readMVar (connThread curConn)
  touch cookie
    where
      guid = grepID selector

unregister :: WarpGrepServer -> GUID -> IO ()
unregister srv guid = do
  notice (wgsLogger srv) (printf "unregistering query: %s" (show guid))
  mconn <- atomically $ do
    m <- readTVar (wgsIOThreads srv)
    case (M.lookup guid m) of
      Nothing   -> return Nothing
      Just conn -> do
        active <- readTVar (connCount conn)
        writeTVar (connCount conn) (active - 1)
        if (active == 1)
          then do
            modifyTVar' (wgsIOThreads srv) (M.delete guid)
            return $ Just conn
          else return Nothing
  case mconn of
    Nothing   -> return ()
    Just conn -> do
      notice (wgsLogger srv) (printf "stopping distribution point/iothread: %s" (show guid))
      (cookie, pid) <- readMVar (connThread conn)
      purge cookie
      killThread pid
      takeMVar (connWaitT conn)
      case (connEvent conn) of
        Left dist  -> do
          detach (gTree $ wgsEventTree srv) dist
          stopDistribution dist
        Right dist -> do
          detach (aTree $ wgsEventTree srv) dist
          stopDistribution dist

forkIOThread :: WarpGrepServer -> GUID -> Connection -> IO ThreadId
forkIOThread srv guid conn =
  forkFinally (either (worker publishG) (worker publishA) (connEvent conn)) (\_ -> putMVar (connWaitT conn) ())
    where
      publishA sink = do
        takeMVar sink >>= mapM_ (wgsStdout srv guid . Right)

      publishG sink = do
        takeMVar sink >>= mapM_ (wgsStdout srv guid . Left)

      worker writer dist =
        bracket (subscribe dist) (\s -> unsubscribe dist s >> void (tryTakeMVar s)) (forever . writer)

warpGrepServer :: WarpGrepServer -> Context -> IO (Pipe Pull)
warpGrepServer wgsrv ctx = do
  pipe <- createPullPipe ctx (wgsLogger wgsrv)
  void $ forkIO (forever $ pipeFeed wgsrv pipe)
  return pipe
