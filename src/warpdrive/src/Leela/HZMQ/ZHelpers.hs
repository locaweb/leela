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

module Leela.HZMQ.ZHelpers where

import           Data.Int
import           System.ZMQ4
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

recvTimeout :: (Receiver a) => Int64 -> Socket a -> IO (Maybe [B.ByteString])
recvTimeout t s = do
  ready <- poll t [Sock s [In] Nothing]
  case ready of
    [[]] -> return Nothing
    _    -> fmap Just (receiveMulti s)

sndTimeout' :: (Sender a) => Int64 -> Socket a -> [L.ByteString] -> IO Bool
sndTimeout' t fh m = do
  ready <- poll t [Sock fh [Out] Nothing]
  case ready of
    [[]] -> return False
    _    -> sendAll' fh m >> return True

sndTimeout :: (Sender a) => Int64 -> Socket a -> [B.ByteString] -> IO Bool
sndTimeout t fh m = do
  ready <- poll t [Sock fh [Out] Nothing]
  case ready of
    [[]] -> return False
    _    -> sendAll fh m >> return True

sendAll' :: (Sender a) => Socket a -> [L.ByteString] -> IO ()
sendAll' _ []         = return ()
sendAll' fh [chk]     = send' fh [] chk
sendAll' fh (chk:msg) = do
  send' fh [SendMore] chk
  sendAll' fh msg

sendAll :: (Sender a) => Socket a -> [B.ByteString] -> IO ()
sendAll _ []         = return ()
sendAll fh [chk]     = send fh [] chk
sendAll fh (chk:msg) = do
  send fh [SendMore] chk
  sendAll fh msg

ms :: Int -> Int
ms = (* 1000)

configAndConnect :: (Int, Int) -> Socket a -> String -> IO ()
configAndConnect (rcvQueue, sndQueue) fh addr = do
  setLinger (restrict 0) fh
  setSendHighWM (restrict sndQueue) fh
  setSendTimeout (restrict (ms 60)) fh
  setTcpKeepAlive On fh
  setReceiveHighWM (restrict rcvQueue) fh
  setReceiveTimeout (restrict (ms 60)) fh
  setMaxMessageSize (restrict (1024 * 1024 :: Int)) fh
  setTcpKeepAliveIdle (restrict 30) fh
  setReconnectInterval (restrict (ms 250)) fh
  connect fh addr

configAndBind :: (Int, Int) -> Socket a -> String -> IO ()
configAndBind (rcvQueue, sndQueue) fh addr = do
  setLinger (restrict 0) fh
  setSendHighWM (restrict sndQueue) fh
  setSendTimeout (restrict (ms 60)) fh
  setTcpKeepAlive On fh
  setReceiveHighWM (restrict rcvQueue) fh
  setReceiveTimeout (restrict (ms 60)) fh
  setMaxMessageSize (restrict (1024 * 1024 :: Int)) fh
  setTcpKeepAliveIdle (restrict 30) fh
  setReconnectInterval (restrict (ms 250)) fh
  bind fh addr
