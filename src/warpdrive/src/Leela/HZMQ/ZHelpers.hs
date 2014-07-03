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

import           System.ZMQ4
import           Control.Monad
import           Leela.Data.Time
import qualified Data.ByteString as B
import           Control.Concurrent
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

retryUnless :: TVar Bool -> a -> STM a
retryUnless tvar a = do
  expired <- readTVar tvar
  if expired then retry else return a

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

setHWM :: (Int, Int) -> Socket a -> IO ()
setHWM (rcvQueue, sndQueue) fh = do
  setReceiveHighWM (restrict rcvQueue) fh
  setSendHighWM (restrict sndQueue) fh

config :: Socket a -> IO ()
config fh = do
  setHWM (0, 0) fh
  setLinger (restrict 0) fh
  setTcpKeepAlive On fh
--  setImmediate True fh

configAndConnect :: Socket a -> String -> IO ()
configAndConnect fh addr = do
  config fh
  connect fh addr

configAndBind :: Socket a -> String -> IO ()
configAndBind fh addr = do
  config fh
  bind fh addr
