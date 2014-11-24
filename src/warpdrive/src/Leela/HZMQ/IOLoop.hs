{-# LANGUAGE FlexibleInstances #-}

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
       , pollLoop
       , newIOLoop
       , useSocket
       , newIOLoop_
       , pollInLoop
       , pollOutLoop
       ) where

import           Data.IORef
import           Data.Maybe
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import           Control.DeepSeq
import qualified Data.ByteString as B
import           Control.Concurrent
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L

type SendCallback a = Socket a -> [B.ByteString] -> IO ()
type RecvCallback a = Socket a -> IO [B.ByteString]

data Poller a = Poller { pollName :: String
                       , pollInQ  :: MVar [[B.ByteString]]
                       , pollOutQ :: Chan ([B.ByteString], IORef Bool)
                       , pollLock :: MVar (Socket a)
                       , pollCtrl :: MVar Bool
                       }

data PollMode = PollRdonlyMode
              | PollWronlyMode
              | PollRdWrMode
              deriving (Eq)

hasWrite :: PollMode -> Bool
hasWrite PollRdonlyMode = False
hasWrite _              = True

hasRead :: PollMode -> Bool
hasRead PollWronlyMode = False
hasRead _              = True

newIOLoop :: String -> MVar [[B.ByteString]] -> Chan ([B.ByteString], IORef Bool) -> Socket a -> IO (Poller a)
newIOLoop name qsrc qdst fh = do
  ctrl <- newEmptyMVar
  lock <- newMVar fh
  return (Poller name qsrc qdst lock ctrl)

newIOLoop_ :: String -> Socket a -> IO (Poller a)
newIOLoop_ name fh = do
  qsrc <- newMVar []
  qdst <- newChan
  newIOLoop name qsrc qdst fh

alive :: Poller a -> IO Bool
alive p = liftM (maybe True id) (tryReadMVar (pollCtrl p))

enqueueD :: Poller a -> ([B.ByteString], IORef Bool) -> IO Bool
enqueueD p a = do
  writeChan (pollOutQ p) a
  return True

enqueueS :: Poller a -> [[B.ByteString]] -> IO Bool
enqueueS _ []   = return False
enqueueS p msgs = do
  putMVar (pollInQ p) (filter checkMsg msgs)
  return True
    where
      checkMsg msg =
        case (break B.null msg) of
          (_, (_ : [])) -> False
          _             -> True

dequeue :: Poller a -> Maybe ([B.ByteString], IORef Bool) -> IO ([B.ByteString], IORef Bool)
dequeue _ (Just acc) = return acc
dequeue p _          = readChan (pollOutQ p)

recvMsg :: (Receiver a) => Poller a -> IO [[B.ByteString]]
recvMsg p = takeMVar (pollInQ p)
 
sendMsg' :: (Sender a) => Logger -> Poller a -> [B.ByteString] -> IO (Maybe (IO ()))
sendMsg' syslog p msg = do
  ref <- newIORef True
  ok  <- msg `deepseq` enqueueD p (msg, ref)
  if ok
   then return $ Just (atomicWriteIORef ref False)
   else do
     warning syslog (printf "dropping message [%s#no space left]" (pollName p))
     return Nothing

sendMsg :: (Sender a) => Logger -> Poller a -> [L.ByteString] -> IO (Maybe (IO ()))
sendMsg syslog p = sendMsg' syslog p . map L.toStrict

sendMsg_ :: (Sender a) => Logger -> Poller a -> [L.ByteString] -> IO Bool
sendMsg_ syslog p = liftM isJust . sendMsg syslog p

cancel :: Poller a -> IO ()
cancel p = void $ tryPutMVar (pollCtrl p) True

useSocket :: Poller a -> (Socket a -> IO b) -> IO b
useSocket p = withMVar (pollLock p)

pollOutLoop :: (Sender a) => Logger -> Poller a -> IO ()
pollOutLoop syslog = gpollLoop syslog PollWronlyMode (const $ return []) sendAll

pollInLoop :: (Receiver a) => Logger -> Poller a -> IO ()
pollInLoop syslog = gpollLoop syslog PollRdonlyMode receiveMulti (\_ _ -> return ())

pollLoop :: (Receiver a, Sender a) => Logger -> Poller a -> IO ()
pollLoop syslog = gpollLoop syslog PollRdWrMode receiveMulti sendAll

gpollLoop :: Logger -> PollMode -> RecvCallback a -> SendCallback a -> Poller a -> IO ()
gpollLoop logger mode recvCallback sendCallback p = go
    where
      handleRecv acc
        | length acc == 32 = enqueueS p acc >> handleRecv []
        | otherwise        = do
          mmsg <- useSocket p readMsg
          case mmsg of
            Just msg ->
              handleRecv (msg : acc)
            Nothing  ->
              enqueueS p acc
          where
            readMsg fh = do
              zready <- events fh
              if (In `elem` zready)
                then fmap Just $ recvCallback fh
                else return Nothing

      handleSend state = do
        msg  <- dequeue p state
        rest <- useSocket p (sendMsg msg)
        case rest of
          Nothing -> handleSend rest
          _       -> return rest
          where
            sendMsg (msg, ref) fh = do
              zready <- events fh
              if (Out `elem` zready)
                then do
                  live <- readIORef ref
                  if live
                    then sendCallback fh msg
                    else warning logger (printf "dropping message [%s#cancel]" (pollName p))
                  return Nothing
                else return $ Just (msg, ref)

      goWrite fh miss = do
        threadWaitWrite fh
        handleSend miss >>= goWrite fh

      goRead fh = do
        threadWaitRead fh
        handleRecv []
        goRead fh

      go = do
        fh  <- useSocket p fileDescriptor
        t0  <- forkIO (when (hasRead mode) $ goRead fh)
        t1  <- forkIO (when (hasWrite mode) $ goWrite fh Nothing)
        modifyMVar_ (pollCtrl p) (\_ -> return False)
        mapM_ killThread [t0, t1]

