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

module Leela.HZMQ.Pipe
       ( Pipe ()
       , PipeConf (..)
       , createPushPipe
       , createPullPipe
       , stopPipe
       , push
       , pull
       , broadcast
       , pipeBind
       , pipeConnect
       ) where

import           Data.IORef
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Pool
import           Leela.Data.Time
import           Leela.HZMQ.IOLoop
import           Control.Concurrent
import           Leela.MonadHelpers
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L

data PipeConf = PipeConf { resources :: IO [Endpoint] }

data ValveST = OnState
             | OffState
             | FailState !Int !Int
             deriving (Eq)

data Pipe a = Pipe { pPool  :: Pool Endpoint ()
                   , valve  :: IORef ValveST
                   , logger :: Logger
                   , engine :: IOLoop a
                   }

createPushPipe :: Context -> Logger -> IO (Pipe Push)
createPushPipe ctx syslog = do
  active <- newIORef OnState
  fh     <- zmqSocket
  ioloop <- newIOLoop_ "pipe#push" fh 100
  _      <- forkFinally (pollOutLoop syslog ioloop) (\_ -> close fh)
  pool   <- createPool (connectTo syslog ioloop) (\e -> const $ disconnectFrom syslog ioloop e)
  return $ Pipe pool active syslog ioloop
    where
      zmqSocket = do
        fh <- socket ctx Push
        setHWM (100, 100) fh
        config fh
        return fh

createPullPipe :: Context -> Logger -> IO (Pipe Pull)
createPullPipe ctx syslog = do
  active <- newIORef OnState
  fh     <- zmqSocket
  ioloop <- newIOLoop_ "pipe#pull" fh 0
  _      <- forkFinally (pollInLoop syslog ioloop) (\_ -> close fh)
  pool   <- createPool (connectTo syslog ioloop) (\e -> const $ disconnectFrom syslog ioloop e)
  return $ Pipe pool active syslog ioloop
    where
      zmqSocket = do
        fh    <- socket ctx Pull
        setHWM (100, 100) fh
        config fh
        return fh

connectTo :: Logger -> IOLoop a -> Endpoint -> IO ()
connectTo syslog ioloop endpoint = do
  let addr = dumpEndpointStr endpoint
  warning syslog (printf "pipe: connecting to: %s" addr)
  useSocket ioloop (flip connect addr)

disconnectFrom :: Logger -> IOLoop a -> Endpoint -> IO ()
disconnectFrom syslog ioloop endpoint = do
  let addr = dumpEndpointStr endpoint
  warning syslog (printf "pipe: disconnecting from: %s" addr)
  useSocket ioloop (flip disconnect addr)

pipeConnect :: Pipe a -> PipeConf -> IO ()
pipeConnect pipe cfg = do
  resources cfg >>= updatePool (pPool pipe)
  void $ forkIO (foreverWith (alive $ engine pipe)
                             (do sleep 1
                                 items <- resources cfg
                                 if (length items == 0)
                                   then disableWrite pipe
                                   else enableWrite pipe
                                 updatePool (pPool pipe) items))

pipeBind :: Pipe a -> Endpoint -> IO ()
pipeBind pipe endpoint = do
  let addr = dumpEndpointStr endpoint
  warning (logger pipe) (printf "pipe: binding to: %s" addr)
  useSocket (engine pipe) (flip bind addr)

stopPipe :: Pipe a -> IO ()
stopPipe = cancel . engine

push :: Pipe Push -> [L.ByteString] -> IO Bool
push pipe msg = do
  state <- atomicModifyIORef' (valve pipe) (\v -> (selectValve v, selectValve v))
  case state of
    OnState  -> sendMsgWithCtrl (engine pipe) (liftM (== OnState) $ readIORef $ valve pipe) msg >>= handle
    OffState -> return True
    _        -> return False
    where
      handle True  = return True
      handle False = atomicModifyIORef' (valve pipe) (\v -> (moveToFailure v, False))

broadcast :: Context -> Timeout -> Pipe Push -> [L.ByteString] -> IO ()
broadcast ctx maxwait pipe msg = whenEnabled pipe $ useAll (pPool pipe) (mapM_ (flip sendCtrl msg . fst))
    where
      sendCtrl endpoint msg = do
        withSocket ctx Push $ \fh -> do
          configAndConnect fh (dumpEndpointStr endpoint)
          ans <- poll maxwait [Sock fh [Out] Nothing]
          case ans of
            [[Out]] -> void $ sendAll' fh msg
            _       -> return ()

pull :: Pipe Pull -> IO [[B.ByteString]]
pull pipe = recvMsg (engine pipe)

disableWrite :: Pipe a -> IO ()
disableWrite pipe = do
  state <- readIORef (valve pipe)
  unless (state == OffState) (warning (logger pipe) "pipe: disabling writes")
  atomicWriteIORef (valve pipe) OffState

enableWrite :: Pipe a -> IO ()
enableWrite pipe = do
  state <- readIORef (valve pipe)
  unless (state == OnState) (warning (logger pipe) "pipe: enabling writes")
  atomicWriteIORef (valve pipe) OnState

whenEnabled :: Pipe a -> IO () -> IO ()
whenEnabled pipe action = do
  state <- readIORef (valve pipe)
  when (state == OnState) action

moveToFailure :: ValveST -> ValveST
moveToFailure OffState        = OffState
moveToFailure OnState         = FailState 1 0
moveToFailure (FailState _ c) = FailState (2 ^ c) (min 10 (c + 1))

selectValve :: ValveST -> ValveST
selectValve OffState        = OffState
selectValve OnState         = OnState
selectValve (FailState 0 0) = OnState
selectValve (FailState 0 c) = FailState (2 ^ c) (c - 1)
selectValve (FailState n c) = FailState (n - 1) c
