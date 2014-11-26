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
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Data.Serialize
import qualified Data.ByteString as B
import           Leela.Data.Pool
import           Leela.Data.Time
import           Leela.HZMQ.IOLoop
import           Control.Exception
import qualified STMContainers.Map as M
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.Timeout
import           Control.Applicative
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

type JKey = Word16

newtype Job = Job (JKey, MVar [B.ByteString])

data ClientConf a = ClientConf { resources :: IO [Endpoint] }

data Client = Bidirectional { logger  :: Logger
                            , manager :: Manager
                            , dealer  :: Poller Dealer
                            , cstate  :: M.Map JKey Job
                            , counter :: Counter JKey
                            }
            | Unidirectional { logger   :: Logger
                             , manager  :: Manager
                             , pipeline :: Either (Poller Push) (Poller Pull)
                             }

newtype ClientFH a = ClientFH Client

defaultTimeout :: Int
defaultTimeout = 5 * 1000 * 1000

readKey :: Job -> JKey
readKey (Job (v, _)) = v

readJob :: Job -> MVar [B.ByteString]
readJob (Job (_, v)) = v

request :: ClientFH Dealer -> [L.ByteString] -> IO (Maybe [B.ByteString])
request (ClientFH client) msg = bracket acquire release useFunc
    where
      acquire = newJob client msg

      useFunc Nothing         = do
        warning (logger client) "dealer: request queue full"
        return Nothing
      useFunc (Just (job, _)) = do
        withHandle (manager client) defaultTimeout tryCancel (fmap build $ takeMVar ref)
          where
            ref = readJob job

            tryCancel = do
              warning (logger client) "dealer: request timeout"
              void $ tryPutMVar ref []

      build [] = Nothing
      build xs = Just xs

      release Nothing             = return ()
      release (Just (job, abort)) = abort >> delJob client (readKey job)

push :: ClientFH Push -> [L.ByteString] -> IO Bool
push (ClientFH client) msg =
  case (pipeline client) of
    Left poller -> snd <$> sendMsg poller msg
    _           -> return False

pull (ClientFH client) =
  case (pipeline client) of
    Right poller -> recvMsg poller
    _            -> return []

newJob :: Client -> [L.ByteString] -> IO (Maybe (Job, IO ()))
newJob client msg = do
  key         <- next (counter client)
  (abort, ok) <- sendMsg (dealer client) (encodeLazy key : L.empty : msg)
  shmem       <- newEmptyMVar
  if ok
    then do
      let job = Job (key, shmem)
      atomically $ M.insert job key (cstate client)
      return $ Just (job, abort)
    else return Nothing

delJob :: Client -> JKey -> IO ()
delJob client key = atomically $ M.delete key (cstate client)

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
  fh     <- zmqSocket
  client <- Bidirectional syslog
              <$> timeoutManager
              <*> (newIOLoop_ "dealer" fh 10000)
              <*> atomically M.new
              <*> newCounter
  runClient syslog cfg client
    where
      zmqSocket = do
        fh    <- socket ctx Dealer
        setHWM (5, 5) fh
        config fh
        return fh

createPush :: Logger -> ClientConf Push -> Context -> IO (ClientFH Push)
createPush syslog cfg ctx = do
  notice syslog "creating zmq.push"
  fh     <- zmqSocket
  client <- Unidirectional syslog
              <$> timeoutManager
              <*> (Left <$> (newIOLoop_ "pipeline" fh 10000))
  runClient syslog cfg client
    where
      zmqSocket = do
        fh <- socket ctx Push
        setHWM (5, 5) fh
        config fh
        return fh

createPull :: Logger -> ClientConf Pull -> Context -> IO (ClientFH Pull)
createPull syslog cfg ctx = do
  notice syslog "creating zmq.pull"
  fh     <- zmqSocket
  client <- Unidirectional syslog
              <$> timeoutManager
              <*> (Right <$> (newIOLoop_ "pipeline" fh 10000))
  runClient syslog cfg client
    where
      zmqSocket = do
        fh <- socket ctx Pull
        setHWM (5, 5) fh
        config fh
        return fh

runClient :: Logger -> ClientConf a -> Client -> IO (ClientFH a)
runClient syslog cfg client = do
  pool <- createPool (onPoller (createWorker syslog)) (onPoller (destroyWorker syslog))
  forkFinally (go pool) (\_ -> onPoller (`useSocket` close))
  return (ClientFH client)
    where
      go pool = do
        warning syslog "dealer has started"
        poolUpdate cfg pool
        caps <- getNumCapabilities
        t0   <- forkIO (supervise syslog "Dealer#poolUpdate" $
                          foreverWith (onPoller alive) (poolUpdate cfg pool >> sleep 1))
        ts   <- mapM (\i -> forkOn i recvLoop) [0..caps-1]
        _    <- ioloop `finally` (mapM_ killThread (t0 : ts))
        warning syslog "dealer has quit"
        
      recvAns msg =
        case (break B.null msg) of
          (key@(_:_), (_:ans)) -> do
            value <- atomically $ maybe (return Nothing) (flip M.lookup (cstate client)) (decodeKey $ last key)
            case value of
              Just job -> reply job ans
              Nothing  -> return ()
          _                    -> return ()

      recvLoop =
        case client of
          Bidirectional _ _ p _ _ -> do
            recvMsg p >>= mapM_ recvAns
            recvLoop
          _                       -> return ()


      onPoller :: forall b. (forall a. Poller a -> b) -> b
      onPoller f = case client of
                     Bidirectional _ _ p _ _ -> f p
                     Unidirectional _ _ p    -> either f f p

      ioloop = case client of
                 Bidirectional _ _ p _ _ -> pollLoop (logger client) p
                 Unidirectional _ _ p    -> either (pollOutLoop (logger client)) (pollInLoop (logger client)) p

stopDealer :: ClientFH a -> IO ()
stopDealer (ClientFH client) = do
  case client of
    Bidirectional {}  -> cancel (dealer client)
    Unidirectional {} -> either cancel cancel (pipeline client)
