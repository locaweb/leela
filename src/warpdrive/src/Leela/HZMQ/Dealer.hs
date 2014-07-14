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
       , create
       , request
       , stopDealer
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
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

type JKey = Word32

newtype Job = Job (JKey, MVar [B.ByteString])

data ClientConf a = ClientConf { endpoint :: (a, a -> IO [Endpoint]) }

data Client = Client { logger  :: Logger
                     , poller  :: [Poller Dealer]
                     , cstate  :: IORef (M.HashMap JKey Job)
                     , counter :: Counter JKey
                     }

newtype ClientFH = ClientFH (Client, IO ())

readKey :: Job -> JKey
readKey (Job (v, _)) = v

readJob :: Job -> MVar [B.ByteString]
readJob (Job (_, v)) = v

modifyState :: IORef a -> (a -> Maybe a) -> IO ()
modifyState ioref f = atomicModifyIORef' ioref $ \state ->
  case (f state) of
   Nothing     -> (state, ())
   Just state' -> (state', ())

request :: Int -> ClientFH -> [L.ByteString] -> IO (Maybe [B.ByteString])
request maxwait (ClientFH (dealer, _)) msg = mask $ \restore -> do
  job <- newJob dealer
  sendMsg (head $ poller dealer) (encodeLazy (readKey job) : L.empty : msg)
  restore (timeout (maxwait * 1000) (takeMVar (readJob job))) `finally` (delJob dealer (readKey job))

newJob :: Client -> IO Job
newJob dealer = do
  key   <- next (counter dealer)
  shmem <- newEmptyMVar
  let job = Job (key, shmem)
  modifyState (cstate dealer) (Just . M.insert key job)
  return job

delJob :: Client -> JKey -> IO ()
delJob dealer key = modifyState (cstate dealer) (Just . M.delete key)

reply :: Job -> [B.ByteString] -> IO ()
reply (Job (_, mvar)) ans = void $ tryPutMVar mvar ans

decodeKey :: B.ByteString -> Maybe JKey
decodeKey = either (const Nothing) Just . decode

dealerLoop :: Client -> IO ()
dealerLoop dealer = do
  warning (logger dealer) "dealer has started"
  wait   <- newQSem 0
  _      <- forkFinally recvLoop (const $ signalQSem wait)
  mapM_ (flip forkFinally (const $ signalQSem wait) . pollLoop) (poller dealer)
  replicateM_ (length (poller dealer) + 1) (waitQSem wait)
  warning (logger dealer) "dealer has quit"
    where
      recvAns msg = do
        case (break B.null msg) of
          (key@(_:_), (_:ans)) -> do
            state <- readIORef (cstate dealer)
            case (decodeKey (last key) >>= flip M.lookup state) of
              Just job -> reply job ans
              Nothing  -> return ()
          _                    -> return ()

      recvLoop = do
        msg <- recvMsg (head $ poller dealer)
        when (isJust msg) (recvAns (fromJust msg) >> recvLoop)

poolUpdate :: ClientConf a -> Pool Endpoint () -> IO ()
poolUpdate cfg pool = do
  let (db, readF) = endpoint cfg
  updatePool pool =<< readF db

create :: Logger -> ClientConf a -> Context -> IO ClientFH
create syslog cfg ctx = do
  notice syslog "creating zmq.dealer"
  caps   <- fmap (max 1) getNumCapabilities
  fhs    <- replicateM caps $ do
    fh <- socket ctx Dealer
    config fh
    return fh
  qsrc   <- newTChanIO
  qdst   <- newTChanIO
  qsem   <- newQSem 0
  dealer <- liftM3 (Client syslog) (mapM (newIOLoop qsrc qdst) fhs) (newIORef M.empty) newCounter
  pool   <- createPool (createWorker dealer) (destroyWorker dealer)
  poolUpdate cfg pool
  _      <- forkFinally
              (supervise syslog "poolUpdate" $ foreverWith (alive (head $ poller dealer)) (poolUpdate cfg pool >> sleep 1))
              (const $ signalQSem qsem)
  _      <- forkFinally (dealerLoop dealer) (const $ signalQSem qsem)
  return $ ClientFH (dealer, replicateM_ 2 (waitQSem qsem) >> mapM_ close fhs)
    where
      destroyWorker dealer addr _ = do
        let addrStr = dumpEndpointStr addr
        forM_ (poller dealer) (\p -> useSocket p (flip disconnect addrStr))
        warning syslog (printf "dealer: disconnect %s" addrStr)

      createWorker dealer addr = do
        let addrStr = dumpEndpointStr addr
        forM_ (poller dealer) (\p -> useSocket p (flip connect  addrStr))
        warning syslog (printf "dealer: connect %s" addrStr)

stopDealer :: ClientFH -> IO ()
stopDealer (ClientFH (dealer, waitStop)) = do
  mapM_ cancel (poller dealer)
  waitStop
