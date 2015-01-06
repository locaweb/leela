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
       ( Client ()
       , ClientConf (..)
       , Push
       , Dealer
       , request
       , stopDealer
       , createDealer
       , dealerConnect
       ) where

import           Data.Word
import           System.ZMQ4
import           Leela.Logger
import           Control.Monad
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
import           Leela.MonadHelpers
import           Control.Applicative
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

type JKey = Word16

newtype Job = Job (JKey, MVar [B.ByteString])

data ClientConf = ClientConf { resources :: IO [Endpoint] }

data Client = Client { logger  :: Logger
                     , manager :: TimeoutManager
                     , engine  :: IOLoop Dealer
                     , cstate  :: M.Map JKey Job
                     , counter :: Counter JKey
                     }

defaultTimeout :: Int
defaultTimeout = 10 * 1000 * 1000

readKey :: Job -> JKey
readKey (Job (v, _)) = v

readJob :: Job -> MVar [B.ByteString]
readJob (Job (_, v)) = v

request :: Client -> [L.ByteString] -> IO (Maybe [B.ByteString])
request client msg = bracket acquire release useFunc
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

newJob :: Client -> [L.ByteString] -> IO (Maybe (Job, IO ()))
newJob client msg = do
  key         <- next (counter client)
  (abort, ok) <- sendMsg (engine client) (encodeLazy key : L.empty : msg)
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

poolUpdate :: ClientConf -> Pool Endpoint () -> IO ()
poolUpdate cfg pool =
  resources cfg >>= updatePool pool

destroyWorker :: Logger -> IOLoop a -> Endpoint -> b -> IO ()
destroyWorker syslog ioloop addr _ = do
  let addrStr = dumpEndpointStr addr
  useSocket ioloop (`disconnect` addrStr)
  warning syslog (printf "dealer: disconnect %s" addrStr)

createWorker :: Logger -> IOLoop a -> Endpoint -> IO ()
createWorker syslog ioloop addr = do
  let addrStr = dumpEndpointStr addr
  useSocket ioloop (`connect` addrStr)
  warning syslog (printf "dealer: connect %s" addrStr)

createDealer :: Logger -> Context -> IO Client
createDealer syslog ctx = do
  notice syslog "creating zmq.dealer"
  fh <- zmqSocket
  Client syslog <$> timeoutManager
                <*> (newIOLoop_ "dealer" fh 10000)
                <*> atomically M.new
                <*> newCounter
    where
      zmqSocket = do
        fh    <- socket ctx Dealer
        setHWM (5, 5) fh
        config fh
        return fh

dealerConnect :: Logger -> Client -> ClientConf -> IO ()
dealerConnect syslog client cfg = do
  pool <- createPool (createWorker syslog (engine client)) (destroyWorker syslog (engine client))
  void $ forkFinally (go pool) (\_ -> engine client `useSocket` close)
    where
      go pool = do
        warning syslog "dealer has started"
        poolUpdate cfg pool
        caps <- getNumCapabilities
        pid  <- forkIO (foreverWith (alive $ engine client) (sleep 1 >> poolUpdate cfg pool))
        pids <- replicateM caps (forkIO (forever $ recvLoop))
        _    <- ioloop `finally` (mapM_ killThread (pid : pids))
        warning syslog "dealer has quit"

      recvAns msg =
        case (break B.null msg) of
          (key@(_:_), (_:ans)) -> do
            value <- atomically $ maybe (return Nothing) (flip M.lookup (cstate client)) (decodeKey $ last key)
            case value of
              Just job -> reply job ans
              Nothing  -> return ()
          _                    -> return ()

      recvLoop = recvMsg (engine client) >>= mapM_ recvAns

      ioloop = pollLoop (logger client) (engine client)

stopDealer :: Client -> IO ()
stopDealer client = cancel (engine client)
