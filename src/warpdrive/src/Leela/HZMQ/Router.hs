{-# LANGUAGE OverloadedStrings #-}

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

module Leela.HZMQ.Router
       ( Worker (..)
       , RouterFH ()
       , stopRouter
       , startRouter
       ) where

import           Data.Maybe
import           System.ZMQ4
import           Data.UUID.V4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Leela.Data.Time
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

data Request = Request Time [B.ByteString] [B.ByteString]

data Worker = Worker { onJob :: [B.ByteString] -> IO [L.ByteString]
                     , onErr :: SomeException -> IO [L.ByteString]
                     }

newtype RouterFH = RouterFH (TVar Bool)

readMsg :: Request -> [B.ByteString]
readMsg (Request _ _ val) = val

readPeer :: Request -> [B.ByteString]
readPeer (Request _ val _) = val

reqTime :: Request -> Time
reqTime (Request val _ _) = val

logresult :: Logger -> Request -> Maybe SomeException -> IO ()
logresult syslog job me = do
  elapsed <- fmap (`diff` (reqTime job)) now
  debug syslog $ printf "%s [%s]" (failOrSucc me) (showDouble $ milliseconds elapsed)
    where
      failOrSucc :: Maybe SomeException -> String
      failOrSucc Nothing  = "ROUTER.ok"
      failOrSucc (Just e) = printf "ROUTER.fail[%s]" (show e)

reply :: Request -> Socket Push -> [L.ByteString] -> IO ()
reply job fh msg = do
  mapM_ (send fh [SendMore]) (readPeer job)
  send' fh [SendMore] L.empty
  sendAll' fh msg

worker :: Logger -> Request -> Socket Push -> Worker -> IO ()
worker syslog job fh action = do
  mmsg <- try (onJob action (readMsg job))
  case mmsg of
    Left e    -> do
      logresult syslog job (Just e)
      msg <- onErr action e
      reply job fh msg
    Right msg -> do
      logresult syslog job Nothing
      reply job fh msg

forkWorker :: Logger -> Context -> String -> Request -> Worker -> IO ()
forkWorker syslog ctx addr job action = void (forkIO $
  withSocket ctx Push $ \fh -> do
    configAndConnect fh addr
    void $ worker syslog job fh action)

recvRequest :: Receiver a => Socket a -> IO (Maybe Request)
recvRequest fh = do
  mmsg <- receiveMulti fh
  time <- now
  let (peer, msg0) = break B.null mmsg
  case msg0 of
    ("" : msg) -> return (Just (Request time peer msg))
    _          -> return Nothing

stopRouter :: RouterFH -> IO ()
stopRouter (RouterFH ctrl) = do
  atomically $ writeTVar ctrl False
  atomically $ readTVar ctrl >>= flip unless retry

startRouter :: Logger -> Endpoint -> Context -> Worker -> IO RouterFH
startRouter syslog endpoint ctx action = do
  notice syslog (printf "starting zmq.router: %s" (dumpEndpointStr endpoint))
  oaddr <- fmap (printf "inproc://router-%s" . show) nextRandom :: IO String
  ctrl  <- newTVarIO True
  void $ flip forkFinally (\_ -> atomically $ writeTVar ctrl True) $ do
    withSocket ctx Router $ \ifh ->
      withSocket ctx Pull $ \ofh -> do
        setHWM (10000, 1000) ofh
        setHWM (1000, 10000) ifh
        configAndBind ofh oaddr
        configAndBind ifh (dumpEndpointStr endpoint)
        foreverWith (atomically $ readTVar ctrl) $ routingLoop ifh ofh oaddr
  return (RouterFH ctrl)
    where
      procRequest fh oaddr = do
        mreq <- recvRequest fh
        when (isJust mreq) (forkWorker syslog ctx oaddr (fromJust mreq) action)

      routingLoop :: Socket Router -> Socket Pull -> String -> IO ()
      routingLoop ifh ofh oaddr = do
        [ev0, ev1] <- poll 1000 [ Sock ifh [In] Nothing
                                , Sock ofh [In] Nothing
                                ]
        unless (null ev0) (procRequest ifh oaddr)
        unless (null ev1) (receiveMulti ofh >>= void . sndTimeout 0 ifh)
