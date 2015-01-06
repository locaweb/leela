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

import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import qualified Data.ByteString as B
import           Control.Exception
import           Leela.HZMQ.IOLoop
import           Control.Concurrent
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L

data Request = Request [B.ByteString] [B.ByteString]

data Worker = Worker { onJob :: [B.ByteString] -> ([L.ByteString] -> IO ()) -> IO ()
                     , onErr :: SomeException -> IO [L.ByteString]
                     }

newtype RouterFH = RouterFH (IOLoop Router)

readMsg :: Request -> [B.ByteString]
readMsg (Request _ val) = val

readPeer :: Request -> [B.ByteString]
readPeer (Request val _) = val

reply :: IOLoop Router -> Request -> [L.ByteString] -> IO ()
reply poller job msg = do
  let ans = (map L.fromStrict $ readPeer job) ++ (L.empty : msg)
  void $ sendMsg_ poller ans

runJob :: IOLoop Router -> Request -> Worker -> IO ()
runJob poller job action = do
  ans <- try (onJob action (readMsg job) (reply poller job))
  atEnd ans
    where
      atEnd (Right _) = return ()
      atEnd (Left e)  = onErr action e >>= reply poller job

recvRequest :: [B.ByteString] -> (Maybe Request)
recvRequest []   = Nothing
recvRequest mmsg = do
  let (peer, msg0) = break B.null mmsg
  case msg0 of
    ("" : msg) -> Just (Request peer msg)
    _          -> Nothing

stopRouter :: RouterFH -> IO ()
stopRouter (RouterFH p) = cancel p

startRouter :: Logger -> Endpoint -> Context -> Worker -> IO RouterFH
startRouter syslog endpoint ctx action = do
  notice syslog (printf "starting zmq.router: %s" (dumpEndpointStr endpoint))
  fh     <- zmqSocket
  poller <- newIOLoop_ "router" fh 10000
  _      <- forkFinally (go poller) (\_ -> close fh)
  return (RouterFH (poller))
    where
      zmqSocket = do
        fh <- socket ctx Router
        setHWM (5, 5) fh
        configAndBind fh (dumpEndpointStr endpoint)
        return fh

      acceptReq poller msg = do
        case (recvRequest msg) of
          Nothing  -> return ()
          Just req -> runJob poller req action

      acceptReqs poller = mapM_ (acceptReq poller)

      recvLoop poller = do
        recvMsg poller >>= acceptReqs poller
        recvLoop poller

      go poller = do
        warning syslog "router has started"
        caps <- getNumCapabilities
        pids <- replicateM caps (forkIO (recvLoop poller))
        pollLoop syslog poller `finally` (mapM_ killThread pids)
        warning syslog "router has quit"
