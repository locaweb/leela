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

newtype RouterFH = RouterFH (Poller Router)

readMsg :: Request -> [B.ByteString]
readMsg (Request _ val) = val

readPeer :: Request -> [B.ByteString]
readPeer (Request val _) = val

reply :: Logger -> Poller Router -> Request -> [L.ByteString] -> IO ()
reply syslog poller job msg = do
  let ans = (map L.fromStrict $ readPeer job) ++ (L.empty : msg)
  void $ sendMsg_ syslog poller ans

worker :: Logger -> Poller Router -> Request -> Worker -> IO ()
worker syslog poller job action = do
  mmsg <- try (onJob action (readMsg job) (reply syslog poller job))
  case mmsg of
    Left e   -> putStrLn (show e) >> onErr action e >>= reply syslog poller job
    Right () -> return ()

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
  poller <- newIOLoop_ "router" fh
  _      <- forkFinally (go poller) (\_ -> close fh)
  return (RouterFH poller)
    where
      zmqSocket = do
        fh <- socket ctx Router
        setHWM (1000, 1000) fh
        configAndBind fh (dumpEndpointStr endpoint)
        return fh

      acceptReq poller msg = do
        let mreq = recvRequest msg
        when (isJust mreq) (worker syslog poller (fromJust mreq) action)

      acceptReqs poller = mapM_ (acceptReq poller)

      recvLoop poller = do
        recvMsg poller >>= acceptReqs poller
        recvLoop poller

      go poller = do
        warning syslog "router has started"
        t <- forkIO (recvLoop poller)
        pollLoop syslog poller `finally` (killThread t)
        warning syslog "router has quit"
