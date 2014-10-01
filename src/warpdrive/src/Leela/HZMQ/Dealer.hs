{-# LANGUAGE Rank2Types        #-}
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
       , pull
       , request
       , stopDealer
       , createDealer
       , createPush
       , createPull
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

type JKey = Word16

newtype Job = Job (JKey, MVar [B.ByteString])

data ClientConf a = ClientConf { resources :: IO [Endpoint] }

data Client = Bidirectional { logger  :: Logger
                            , dealer  :: Poller Dealer
                            , cstate  :: IORef (M.HashMap JKey Job)
                            , counter :: Counter JKey
                            }
            | Unidirectional { logger   :: Logger
                             , pipeline :: Either (Poller Push) (Poller Pull)
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
push (ClientFH (client, _)) msg =
  case (pipeline client) of
    Left poller -> sendMsg_ (logger client) poller msg
    _           -> return False

pull (ClientFH (client, _)) =
  case (pipeline client) of
    Right poller -> recvMsg poller
    _            -> return Nothing

newJob :: Client -> [L.ByteString] -> IO (Maybe (Job, IO ()))
newJob client msg = do
  key   <- next (counter client)
  abort <- sendMsg (logger client) (dealer client) (encodeLazy key : L.empty : msg)
  case abort of
    Nothing   -> return Nothing
    Just func -> do
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
  client <- Bidirectional syslog
              <$> (newIOLoop_ "dealer" (caps * 64) (caps * 32) fh)
              <*> (newIORef M.empty)
              <*> newCounter
  runClient syslog cfg client
    where
      zmqSocket = do
        fh   <- socket ctx Dealer
        caps <- fmap (max 1) getNumCapabilities
        setHWM (caps * 8, caps * 8) fh
        config fh
        return fh

createPush :: Logger -> ClientConf Push -> Context -> IO (ClientFH Push)
createPush syslog cfg ctx = do
  notice syslog "creating zmq.push"
  caps   <- fmap (max 1) getNumCapabilities
  fh     <- zmqSocket
  client <- Unidirectional syslog
              <$> Left
              <$> (newIOLoop_ "pipeline" 0 (caps * 8) fh)
  runClient syslog cfg client
    where
      zmqSocket = do
        fh   <- socket ctx Push
        caps <- fmap (max 1) getNumCapabilities
        setHWM (caps * 8, caps * 8) fh
        config fh
        return fh

createPull :: Logger -> ClientConf Pull -> Context -> IO (ClientFH Pull)
createPull syslog cfg ctx = do
  notice syslog "creating zmq.pull"
  caps   <- fmap (max 1) getNumCapabilities
  fh     <- zmqSocket
  client <- Unidirectional syslog
              <$> Right
              <$> (newIOLoop_ "pipeline" (caps * 8) 0 fh)
  runClient syslog cfg client
    where
      zmqSocket = do
        fh   <- socket ctx Pull
        caps <- fmap (max 1) getNumCapabilities
        setHWM (caps * 8, caps * 8) fh
        config fh
        return fh

runClient :: Logger -> ClientConf a -> Client -> IO (ClientFH a)
runClient syslog cfg client = do
  qsem <- newQSemN 0
  pool <- createPool (onPoller (createWorker syslog)) (onPoller (destroyWorker syslog))
  poolUpdate cfg pool
  _    <- forkFinally
            (supervise syslog "startDealer#poolUpdate" $ foreverWith (onPoller alive) (poolUpdate cfg pool >> sleep 1))
            (const $ signalQSemN qsem 1)
  _    <- forkFinally recvLoop (const $ signalQSemN qsem 1)

  _    <- forkFinally ioloop (const $ signalQSemN qsem 1)
  return (ClientFH (client, waitQSemN qsem 3 >> onPoller (`useSocket` close)))
    where
      recvAns msg =
        case (break B.null msg) of
          (key@(_:_), (_:ans)) -> do
            state <- readIORef (cstate client)
            case (decodeKey (last key) >>= flip M.lookup state) of
              Just job -> reply job ans
              Nothing  -> return ()
          _                    -> return ()

      recvLoop =
        case client of
          Bidirectional _ p _ _ -> do
            msg <- recvMsg p
            when (isJust msg) (recvAns (fromJust msg) >> recvLoop)
          _                     -> return ()


      onPoller :: forall b. (forall a. Poller a -> b) -> b
      onPoller f = case client of
                     Bidirectional _ p _ _ -> f p
                     Unidirectional _ p    -> either f f p

      ioloop = case client of
                 Bidirectional _ p _ _ -> pollLoop (logger client) p
                 Unidirectional _ p    -> either (pollOutLoop (logger client)) (pollInLoop (logger client)) p

stopDealer :: ClientFH a -> IO ()
stopDealer (ClientFH (client, waitStop)) = do
  case client of
    Bidirectional {}  -> cancel (dealer client)
    Unidirectional {} -> either cancel cancel (pipeline client)
  waitStop
