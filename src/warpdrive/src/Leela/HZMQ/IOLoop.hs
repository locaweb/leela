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
       ( IOLoop ()
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
       , sendMsgWithCtrl
       , sendMsgWithCtrl'
       ) where

import           Data.Maybe
import           Data.IORef
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import           Control.DeepSeq
import qualified Data.ByteString as B
import           Control.Concurrent
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.BoundedChan as C

type SendCallback a = Socket a -> [B.ByteString] -> IO ()

type RecvCallback a = Socket a -> IO [B.ByteString]

type CancelAction = IO ()

data IOLoop a = IOLoop { pollName  :: String
                       , pollInQ   :: MVar [[B.ByteString]]
                       , pollOutQ  :: BoundedChan ([B.ByteString], IO Bool)
                       , pollLock  :: MVar (Socket a)
                       , pollCtrl  :: MVar ()
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

newIOLoop :: String -> MVar [[B.ByteString]] -> BoundedChan ([B.ByteString], IO Bool) -> Socket a -> IO (IOLoop a)
newIOLoop name qsrc qdst fh = do
  ctrl  <- newEmptyMVar
  lock  <- newMVar fh
  return (IOLoop name qsrc qdst lock ctrl)

newIOLoop_ :: String -> Socket a -> Int -> IO (IOLoop a)
newIOLoop_ name fh limit = do
  qsrc <- newMVar []
  qdst <- newBoundedChan limit
  newIOLoop name qsrc qdst fh

alive :: IOLoop a -> IO Bool
alive p = liftM isNothing (tryReadMVar (pollCtrl p))

enqueueD :: IOLoop a -> ([B.ByteString], IO Bool) -> IO Bool
enqueueD p a = tryWriteChan (pollOutQ p) a

enqueueS :: IOLoop a -> [[B.ByteString]] -> IO Bool
enqueueS p msgs =
  case (filter checkMsg msgs) of
    []    -> return False
    msgs' -> putMVar (pollInQ p) msgs' >> return True
    where
      checkMsg msg =
        case (break B.null msg) of
          (_, (_ : [])) -> False
          _             -> True

dequeue :: IOLoop a -> Maybe ([B.ByteString], IO Bool) -> IO ([B.ByteString], IO Bool)
dequeue _ (Just acc) = return acc
dequeue p _          = C.readChan (pollOutQ p)

recvMsg :: (Receiver a) => IOLoop a -> IO [[B.ByteString]]
recvMsg p = takeMVar (pollInQ p)
 
sendMsg' :: (Sender a) => IOLoop a -> [B.ByteString] -> IO (CancelAction, Bool)
sendMsg' p msg = do
  ctrl <- newIORef True
  ok   <- sendMsgWithCtrl' p (readIORef ctrl) msg
  return (writeIORef ctrl False, ok)

sendMsg :: (Sender a) => IOLoop a -> [L.ByteString] -> IO (CancelAction, Bool)
sendMsg p = sendMsg' p . map L.toStrict

sendMsg_ :: (Sender a) => IOLoop a -> [L.ByteString] -> IO Bool
sendMsg_ p = sendMsgWithCtrl p (return True)

sendMsgWithCtrl' :: (Sender a) => IOLoop a -> IO Bool -> [B.ByteString] -> IO Bool
sendMsgWithCtrl' p ctrl msg = msg `deepseq` enqueueD p (msg, ctrl)

sendMsgWithCtrl :: (Sender a) => IOLoop a -> IO Bool -> [L.ByteString] -> IO Bool
sendMsgWithCtrl p ctrl = sendMsgWithCtrl' p ctrl . map L.toStrict

cancel :: IOLoop a -> IO ()
cancel p = putMVar (pollCtrl p) ()

useSocket :: IOLoop a -> (Socket a -> IO b) -> IO b
useSocket p = withMVar (pollLock p)

pollOutLoop :: (Sender a) => Logger -> IOLoop a -> IO ()
pollOutLoop syslog = gpollLoop syslog PollWronlyMode (const $ return []) sendAll

pollInLoop :: (Receiver a) => Logger -> IOLoop a -> IO ()
pollInLoop syslog = gpollLoop syslog PollRdonlyMode receiveMulti (\_ _ -> return ())

pollLoop :: (Receiver a, Sender a) => Logger -> IOLoop a -> IO ()
pollLoop syslog = gpollLoop syslog PollRdWrMode receiveMulti sendAll

gpollLoop :: Logger -> PollMode -> RecvCallback a -> SendCallback a -> IOLoop a -> IO ()
gpollLoop logger mode recvCallback sendCallback p = go
    where
      handleRecv acc
        | length acc == 4 = enqueueS p acc >> handleRecv []
        | otherwise       = do
          mmsg <- useSocket p readMsg
          case mmsg of
            Just msg -> handleRecv (msg : acc)
            Nothing  -> enqueueS p acc
          where
            readMsg fh = do
              zready <- events fh
              if (In `elem` zready)
                then fmap Just $ recvCallback fh
                else return Nothing

      handleSend state = do
        (msg, readStatus) <- dequeue p state
        live              <- readStatus
        rest              <- if live
                               then useSocket p (mySendMsg msg readStatus)
                               else do
                                 warning logger (printf "dropping message [%s#cancel]" (pollName p))
                                 return Nothing
        case rest of
          Nothing -> handleSend rest
          _       -> return rest
          where
            mySendMsg msg ctrl fh = do
              zready <- events fh
              if (Out `elem` zready)
                then sendCallback fh msg >> return Nothing
                else return $ Just (msg, ctrl)

      goWrite fh miss = do
        miss' <- handleSend miss
        case miss' of
          Nothing -> goWrite fh miss'
          _       -> threadWaitWrite fh >> goWrite fh miss'

      goRead fh = do
        threadWaitRead fh
        handleRecv []
        goRead fh

      go = do
        fh  <- useSocket p fileDescriptor
        t0  <- forkIO (when (hasRead mode) $ goRead fh)
        t1  <- forkIO (when (hasWrite mode) $ goWrite fh Nothing)
        readMVar (pollCtrl p)
        mapM_ killThread [t0, t1]

