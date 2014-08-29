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

module Leela.HZMQ.Dealer
       ( ClientFH ()
       , ClientConf (..)
       , Push
       , Dealer
       , push
       , request
       , stopDealer
       , createDealer
       , createPipeline
       ) where

import           Data.Word
import           Data.IORef
import           Data.Maybe
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Data.Serialize
import           System.Timeout
import qualified Data.ByteString as B
import           Leela.Data.Pool
import           Leela.Data.Time
import qualified Data.HashMap.Lazy as M
import           Leela.HZMQ.IOLoop
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Counter
import           Control.Applicative
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L

type JKey = Word32

newtype Job = Job (JKey, MVar [B.ByteString])

data ClientConf a = ClientConf { resources :: IO [Endpoint] }

data Client = ReqReq { logger  :: Logger
                     , dealer  :: Poller Dealer
                     , cstate  :: IORef (M.HashMap JKey Job)
                     , counter :: Counter JKey
                     }
            | Pipeline { logger   :: Logger
                       , pipeline :: Poller Push
                       }

newtype ClientFH a = ClientFH (Client, IO ())

readKey :: Job -> JKey
readKey (Job (v, _)) = v

readJob :: Job -> MVar [B.ByteString]
readJob (Job (_, v)) = v

modifyState :: IORef a -> (a -> Maybe a) -> IO ()
modifyState ioref f = atomicModifyIORef' ioref $ \state ->
  case (f state) of
   Nothing     -> (state, ())
   Just state' -> (state', ())

request :: Int -> ClientFH Dealer -> [L.ByteString] -> IO (Maybe [B.ByteString])
request maxwait (ClientFH (client, _)) msg = bracket acquire release useFunc
    where
      acquire = newJob client msg

      useFunc Nothing         = return Nothing
      useFunc (Just (job, _)) = timeout (maxwait * 1000) (takeMVar (readJob job))

      release Nothing             = return ()
      release (Just (job, abort)) = abort >> delJob client (readKey job)

push :: ClientFH Push -> [L.ByteString] -> IO Bool
push (ClientFH (client, _)) msg = do
  sendMsg_ (pipeline client) msg

newJob :: Client -> [L.ByteString] -> IO (Maybe (Job, IO ()))
newJob client msg = do
  key   <- next (counter client)
  abort <- sendMsg (dealer client) (encodeLazy key : L.empty : msg)
  case abort of
    Nothing -> do
      warning (logger client) "dropping request [dealer#sndqueue full]"
      return Nothing
    Just func  -> do
      shmem <- newEmptyMVar
      let job = Job (key, shmem)
      modifyState (cstate client) (Just . M.insert key job)
      return (Just (job, func))

delJob :: Client -> JKey -> IO ()
delJob client key = modifyState (cstate client) (Just . M.delete key)

reply :: Job -> [B.ByteString] -> IO ()
reply (Job (_, mvar)) ans = void $ tryPutMVar mvar ans

decodeKey :: B.ByteString -> Maybe JKey
decodeKey = either (const Nothing) Just . decode

dealerLoop :: Client -> IO ()
dealerLoop client = do
  warning (logger client) "dealer has started"
  wait   <- newQSemN 0
  _      <- forkFinally recvLoop (const $ signalQSemN wait 1)
  _      <- forkFinally (pollLoop (logger client) (dealer client)) (const $ signalQSemN wait 1)
  waitQSemN wait 2
  warning (logger client) "dealer has quit"
    where
      recvAns msg =
        case (break B.null msg) of
          (key@(_:_), (_:ans)) -> do
            state <- readIORef (cstate client)
            case (decodeKey (last key) >>= flip M.lookup state) of
              Just job -> reply job ans
              Nothing  -> return ()
          _                    -> return ()

      recvLoop = do
        msg <- recvMsg (dealer client)
        when (isJust msg) (recvAns (fromJust msg) >> recvLoop)

pipelineLoop :: Client -> IO ()
pipelineLoop client = do
  warning (logger client) "pipeline has started"
  pollOutLoop (logger client) (pipeline client)
  warning (logger client) "pipeline has quit"

poolUpdate :: ClientConf a -> Pool Endpoint () -> IO ()
poolUpdate cfg pool =
  resources cfg >>= updatePool pool

destroyWorker :: Logger -> Poller a -> Endpoint -> b -> IO ()
destroyWorker syslog ioloop addr _ = do
  let addrStr = dumpEndpointStr addr
  useSocket ioloop (`disconnect` addrStr)
  warning syslog (printf "dealer: disconnect %s" addrStr)

createWorker :: Logger -> Poller a -> Endpoint -> IO ()
createWorker syslog ioloop addr = do
  let addrStr = dumpEndpointStr addr
  useSocket ioloop (`connect` addrStr)
  warning syslog (printf "dealer: connect %s" addrStr)

createDealer :: Logger -> ClientConf Dealer -> Context -> IO (ClientFH Dealer)
createDealer syslog cfg ctx = do
  notice syslog "creating zmq.dealer"
  caps   <- fmap (max 1) getNumCapabilities
  fh     <- zmqSocket
  qsem   <- newQSemN 0
  client <- ReqReq syslog <$> (newIOLoop_ "dealer" (caps * 100) (caps * 1000) fh) <*> (newIORef M.empty) <*> newCounter
  pool   <- createPool (createWorker syslog (dealer client)) (destroyWorker syslog (dealer client))
  poolUpdate cfg pool
  _      <- forkFinally
              (supervise syslog "poolUpdate" $ foreverWith (alive (dealer client)) (poolUpdate cfg pool >> sleep 1))
              (const $ signalQSemN qsem 1)
  _      <- forkFinally (dealerLoop client) (const $ signalQSemN qsem 1)
  return $ ClientFH (client, waitQSemN qsem 2 >> close fh)
    where
      zmqSocket = do
        fh <- socket ctx Dealer
        setHWM (1000, 10) fh
        config fh
        return fh

createPipeline :: Logger -> ClientConf Push -> Context -> IO (ClientFH Push)
createPipeline syslog cfg ctx = do
  notice syslog "creating zmq.pipeline"
  caps   <- fmap (max 1) getNumCapabilities
  fh     <- zmqSocket
  qsem   <- newQSemN 0
  client <- Pipeline syslog <$> (newIOLoop_ "pipeline" 0 (caps * 100) fh)
  pool   <- createPool (createWorker syslog (pipeline client)) (destroyWorker syslog (pipeline client))
  poolUpdate cfg pool
  _      <- forkFinally
              (supervise syslog "poolUpdate" $ foreverWith (alive (pipeline client)) (poolUpdate cfg pool >> sleep 1))
              (const $ signalQSemN qsem 1)
  _      <- forkFinally (pipelineLoop client) (const $ signalQSemN qsem 1)
  return $ ClientFH (client, waitQSemN qsem 2 >> close fh)
    where
      zmqSocket = do
        fh <- socket ctx Push
        setHWM (0, 1000) fh
        config fh
        return fh

stopDealer :: ClientFH a -> IO ()
stopDealer (ClientFH (client, waitStop)) = do
  case client of
    ReqReq {}   -> cancel (dealer client)
    Pipeline {} -> cancel (pipeline client)
  waitStop
