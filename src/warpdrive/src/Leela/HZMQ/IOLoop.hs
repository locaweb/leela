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

module Leela.HZMQ.IOLoop
       ( Poller ()
       , alive
       , cancel
       , sendMsg
       , recvMsg
       , aliveSTM
       , pollLoop
       , newIOLoop
       , useSocket
       , newIOLoop_
       ) where

import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

newtype Poller a = Poller (TBQueue [B.ByteString], TBQueue [L.ByteString], MVar (Socket a), TVar Bool)

newIOLoop :: TBQueue [B.ByteString] -> TBQueue [L.ByteString] -> Socket a -> IO (Poller a)
newIOLoop qsrc qdst fh = do
  ctrl <- newTVarIO True
  lock <- newMVar fh
  return (Poller (qsrc, qdst, lock, ctrl))

newIOLoop_ :: Socket a -> IO (Poller a)
newIOLoop_ fh = do
  qsrc <- newTBQueueIO 1024
  qdst <- newTBQueueIO 1024
  newIOLoop qsrc qdst fh

aliveSTM :: Poller a -> STM Bool
aliveSTM (Poller (_, _, _, ctrl)) = readTVar ctrl

alive :: Poller a -> IO Bool
alive = atomically . aliveSTM

tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue q a = (writeTBQueue q a >> return True) `orElse` (return False)

enqueue :: Poller a -> [B.ByteString] -> STM Bool
enqueue (Poller (q, _, _, _)) msg =
  case (break B.null msg) of
    (_, (_ : [])) -> return False
    _             -> tryWriteTBQueue q msg

dequeue :: Poller a -> Maybe [L.ByteString] -> STM [L.ByteString]
dequeue _ (Just msg)            = return msg
dequeue (Poller (_, q, _, _)) _ = readTBQueue q

recvMsg :: Poller a -> IO (Maybe [B.ByteString])
recvMsg p@(Poller (q, _, _, _)) = atomically $ do
  ok <- aliveSTM p
  if ok
   then fmap Just (readTBQueue q)
   else return Nothing

sendMsg :: Poller a -> [L.ByteString] -> IO Bool
sendMsg (Poller (_, q, _, _)) msg = atomically $ tryWriteTBQueue q msg

cancel :: Poller a -> IO ()
cancel (Poller (_, _, _, ctrl)) = atomically $ writeTVar ctrl False

useSocket :: Poller a -> (Socket a -> IO b) -> IO b
useSocket (Poller (_, _, lock, _)) = withMVar lock

pollLoop :: (Receiver a, Sender a) => Logger -> Poller a -> IO ()
pollLoop syslog p@(Poller (_, _, _, ctrl)) = do
  fd               <- useSocket p fileDescriptor
  (waitW, cancelW) <- waitFor (threadWaitWrite fd)
  (waitR, cancelR) <- waitFor (threadWaitRead fd)
  supervise syslog "ioloop" (go waitR waitW Nothing) `finally` (cancelR >> cancelW)
    where
      waitFor waitFunc = do
        v <- newEmptyTMVarIO
        t <- forkIO $ supervise syslog "ioloop/waitFor" $ forever $ do
          _ <- waitFunc
          atomically $ putTMVar v ()
        return (takeTMVar v, killThread t)

      handleRecv fh = do
        zready <- events fh
        if (In `elem` zready)
         then do
           ok <- receiveMulti fh >>= atomically . enqueue p
           unless ok (warning syslog "dropping message [rcvqueue full]")
           handleRecv fh
         else return zready

      handleSend msg fh = do
        zready <- events fh
        if (Out `elem` zready)
         then sendAll' fh msg >> return Nothing
         else return (Just msg)
      
      go waitR waitW wMiss = do
        ready <- atomically $ do
          ok <- readTVar ctrl
          if ok
           then fmap Just ((waitW >> fmap Right (dequeue p wMiss)) `orElse` (fmap Left waitR))
           else return Nothing
        case ready of
          Nothing          -> return ()
          Just (Left _)    -> do
            _ <- useSocket p handleRecv
            go waitR waitW wMiss
          Just (Right msg) -> do
            wMiss' <- useSocket p (handleSend msg)
            go waitR waitW wMiss'
