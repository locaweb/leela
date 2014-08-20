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
       , sendMsg_
       , aliveSTM
       , pollLoop
       , newIOLoop
       , useSocket
       , newIOLoop_
       ) where

import           Data.Maybe
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Control.DeepSeq
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

data Poller a = Poller { pollName :: String
                       , pollInQ  :: TBQueue [B.ByteString]
                       , pollOutQ :: TBQueue (MVar [B.ByteString])
                       , pollLock :: MVar (Socket a)
                       , pollCtrl :: TVar Bool
                       }

newIOLoop :: String -> TBQueue [B.ByteString] -> TBQueue (MVar [B.ByteString]) -> Socket a -> IO (Poller a)
newIOLoop name qsrc qdst fh = do
  ctrl <- newTVarIO True
  lock <- newMVar fh
  return (Poller name qsrc qdst lock ctrl)

newIOLoop_ :: String -> Int -> Int -> Socket a -> IO (Poller a)
newIOLoop_ name srcsz dstsz fh = do
  qsrc <- newTBQueueIO srcsz
  qdst <- newTBQueueIO dstsz
  newIOLoop name qsrc qdst fh

aliveSTM :: Poller a -> STM Bool
aliveSTM = readTVar . pollCtrl

alive :: Poller a -> IO Bool
alive = atomically . aliveSTM

tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue q a = (writeTBQueue q a >> return True) `orElse` (return False)

enqueue :: Poller a -> [B.ByteString] -> STM Bool
enqueue p msg =
  case (break B.null msg) of
    (_, (_ : [])) -> return False
    _             -> tryWriteTBQueue (pollInQ p) msg

dequeue :: Poller a -> Maybe [B.ByteString] -> STM (Either [B.ByteString] (MVar [B.ByteString]))
dequeue _ (Just msg) = return (Left msg)
dequeue p _          = liftM Right (readTBQueue (pollOutQ p))

recvMsg :: Poller a -> IO (Maybe [B.ByteString])
recvMsg p = atomically $ do
  ok <- aliveSTM p
  if ok
   then liftM Just (readTBQueue (pollInQ p))
   else return Nothing

sendMsg' :: Poller a -> [B.ByteString] -> IO (Maybe (IO ()))
sendMsg' p msg = do
  mvar <- msg `deepseq` newMVar msg
  ok   <- atomically $ tryWriteTBQueue (pollOutQ p) mvar
  if ok
   then return (Just $ void $ tryTakeMVar mvar)
   else return Nothing

sendMsg :: Poller a -> [L.ByteString] -> IO (Maybe (IO ()))
sendMsg p = sendMsg' p . map L.toStrict

sendMsg_ :: Poller a -> [L.ByteString] -> IO Bool
sendMsg_ p = liftM isJust . sendMsg p

readReq :: Logger -> String -> Either [B.ByteString] (MVar [B.ByteString]) -> IO (Maybe [B.ByteString])
readReq _ _ (Left msg)           = return (Just msg)
readReq syslog name (Right mvar) = do
  mmsg <- tryTakeMVar mvar
  when (isNothing mmsg) $
    warning syslog (printf "dropping message [%s#cancel]" name)
  return mmsg

cancel :: Poller a -> IO ()
cancel = atomically . flip writeTVar False . pollCtrl

useSocket :: Poller a -> (Socket a -> IO b) -> IO b
useSocket p = withMVar (pollLock p)

pollLoop :: (Receiver a, Sender a) => Logger -> Poller a -> IO ()
pollLoop syslog p = do
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
          atomically $ isEmptyTMVar v >>= flip unless retry
        return (takeTMVar v, killThread t)

      handleRecv fh = do
        zready <- events fh
        if (In `elem` zready)
         then do
           ok <- receiveMulti fh >>= atomically . enqueue p
           unless ok (warning syslog (printf "dropping message [%s#rcvqueue full]" (pollName p)))
           handleRecv fh
         else return zready

      handleSend msg fh = do
        zready <- events fh
        if (Out `elem` zready)
         then sendAll fh msg >> return Nothing
         else return (Just msg)

      execPoll waitR waitW wMiss = do
        wready <- (waitW >> liftM Just (dequeue p wMiss)) `orElse` (return Nothing)
        rready <- (waitR >> return True) `orElse` (return False)
        when (rready == False && wready == Nothing) retry
        return (rready, wready)

      go waitR waitW wMiss = do
        ready <- atomically $ do
          ok <- readTVar (pollCtrl p)
          if ok
           then fmap Right (execPoll waitR waitW wMiss)
           else return (Left ())
        case ready of
          Left ()                  -> return ()
          Right (rready, Just req) -> do
            when rready $ void (useSocket p handleRecv)
            mmsg <- readReq syslog (pollName p) req
            case mmsg of
              Nothing  -> go waitR waitW wMiss
              Just msg -> do
                wMiss' <- useSocket p (handleSend msg)
                go waitR waitW wMiss'
          Right (rready, Nothing)  -> do
            when rready $ void (useSocket p handleRecv)
            go waitR waitW wMiss
