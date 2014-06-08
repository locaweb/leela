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
       , startRouter
       ) where

import           Data.Maybe
import           System.ZMQ4
import           Data.UUID.V4
import           Leela.Logger
import           Control.Monad
import           Leela.Data.Time
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers

data Request = Request Time [B.ByteString] [B.ByteString]

data Worker = Worker { onJob :: [B.ByteString] -> IO [B.ByteString]
                     , onErr :: SomeException -> IO [B.ByteString]
                     }

readMsg :: Request -> [B.ByteString]
readMsg (Request _ _ val) = val

readPeer :: Request -> [B.ByteString]
readPeer (Request _ val _) = val

reqTime :: Request -> Time
reqTime (Request val _ _) = val

logresult :: Logger -> Request -> Maybe SomeException -> IO ()
logresult syslog job me = do
  elapsed <- fmap (`diff` (reqTime job)) now
  debug syslog $ printf "%s (%.4fms)" (failOrSucc me) (1000 * elapsed)
    where
      failOrSucc :: Maybe SomeException -> String
      failOrSucc Nothing  = "ROUTER.ok"
      failOrSucc (Just e) = printf "ROUTER.fail[%s]" (show e)

reply :: Request -> Socket Push -> [B.ByteString] -> IO Bool
reply job fh msg = sndTimeout (-1) fh (readPeer job ++ [""] ++ msg)

worker :: Logger -> Request -> Socket Push -> Worker -> IO ()
worker syslog job fh action = do
  mmsg <- try (onJob action (readMsg job))
  case mmsg of
    Left e    -> do
      logresult syslog job (Just e)
      msg <- onErr action e
      void $ reply job fh msg
    Right msg -> do
      logresult syslog job Nothing
      void $ reply job fh msg

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

startRouter :: Logger -> Endpoint -> Context -> Worker -> IO ()
startRouter syslog endpoint ctx action = do
  notice syslog (printf "starting zmq.router: %s" (dumpEndpointStr endpoint))
  oaddr <- fmap (printf "inproc://%s" . show) nextRandom :: IO String
  withSocket ctx Router $ \ifh ->
    withSocket ctx Pull $ \ofh -> do
      configAndBind ofh oaddr
      configAndBind ifh (dumpEndpointStr endpoint)
      forever $ routingLoop ifh ofh oaddr
    where
      procRequest fh oaddr = do
        mreq <- recvRequest fh
        when (isJust mreq) (forkWorker syslog ctx oaddr (fromJust mreq) action)

      routingLoop :: Socket Router -> Socket Pull -> String -> IO ()
      routingLoop ifh ofh oaddr = do
        [ev0, ev1] <- poll (-1) [ Sock ifh [In] Nothing
                                , Sock ofh [In] Nothing
                                ]
        unless (null ev0) (procRequest ifh oaddr)
        unless (null ev1) (receiveMulti ofh >>= void . sndTimeout 1 ifh)

