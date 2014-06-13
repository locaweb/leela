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
       , destroy
       , request
       ) where

import           Data.List (sort)
import           Data.Word
import           Data.Maybe
import           System.ZMQ4 hiding (backlog)
import           Data.UUID.V4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Data.Serialize
import qualified System.Timeout as T
import qualified Data.ByteString as B
import           Leela.Data.Pool
import           Control.Exception
import qualified Data.HashMap.Lazy as H
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

data Job = Job { jmsg  :: [L.ByteString]
               , slot  :: MVar (Maybe [B.ByteString])
               }

data ClientConf a = ClientConf { backlog      :: Int
                               , endpoint     :: (a, a -> IO [Endpoint])
                               , capabilities :: Int
                               }

data Client = Client { logger  :: Logger
                     , queue   :: MVar L.ByteString
                     , state   :: TVar (H.HashMap L.ByteString Job)
                     , counter :: Counter Word64
                     }

newtype ClientFH = ClientFH (Client, Pool [Endpoint] (TVar Bool, Int, QSem), TVar Bool)

request :: Int -> ClientFH -> [L.ByteString] -> IO (Maybe [B.ByteString])
request timeout (ClientFH (dealer, _, _)) a = mask $ \restore -> do
  shmem <- newEmptyMVar
  tick  <- fmap encodeLazy $ next (counter dealer)
  createFH shmem tick
  putMVar (queue dealer) tick
  fmap join (restore (T.timeout (timeout * 1000) (takeMVar shmem)) `finally` (dropFH tick))
    where
      createFH shmem tick = atomically $ do
        m <- readTVar (state dealer)
        writeTVar (state dealer) (H.insert tick (Job a shmem) m)

      dropFH tick = atomically $ do
        m <- readTVar (state dealer)
        writeTVar (state dealer) (H.delete tick m)

readJob :: Client -> L.ByteString -> IO (Maybe Job)
readJob dealer tick = atomically $ fmap (H.lookup tick) (readTVar (state dealer))

reply :: Job -> Maybe [B.ByteString] -> IO ()
reply job ans = putMVar (slot job) ans

dealerLoop :: Context -> Client -> IO Bool -> StrEndpoint -> [Endpoint] -> IO ()
dealerLoop ctx dealer cont iaddr oaddr = do
  withSocket ctx Dealer $ \ofh -> do
    mapM_ (configAndConnect (1000, 10000) ofh . dumpEndpointStr) oaddr
    withSocket ctx Pull $ \ifh -> do
      configAndConnect (10000, 1000) ifh iaddr
      foreverWith cont $ go ifh ofh
        where
          go ifh ofh = do
            [ev0, ev1] <- poll 1000 [ Sock ifh [In] Nothing
                                    , Sock ofh [In] Nothing
                                    ]
            unless (null ev0) $ do
              key  <- fmap L.fromStrict (receive ifh)
              mjob <- readJob dealer key
              when (isJust mjob) $ do
                ok <- sndTimeout' 0 ofh (key : L.empty : (jmsg $ fromJust mjob))
                unless ok $ do
                  warning (logger dealer) "dealer: could not send message"
                  reply (fromJust mjob) Nothing
            unless (null ev1) $ do
              (key, (_:ans)) <- fmap (break B.null) (receiveMulti ofh)
              mmsg           <- readJob dealer (L.fromStrict $ last key)
              when (isJust mmsg) (reply (fromJust mmsg) (Just ans))

create :: Logger -> ClientConf a -> Context -> IO ClientFH
create syslog cfg ctx = do
  notice syslog (printf "creating zmq.dealer [capabilities %d]" (capabilities cfg))
  ctrl   <- newTVarIO True
  iaddr  <- fmap (printf "inproc://%s" . show) nextRandom :: IO String
  queue  <- newEmptyMVar
  dealer <- liftM2 (Client syslog queue) (newTVarIO H.empty) newCounter
  pool   <- createPool (createWorker ctrl dealer iaddr) destroyWorker
  _      <- flip forkFinally (\_ -> atomically $ writeTVar ctrl True) $
    withSocket ctx Push $ \fh -> do
      configAndBind (1000, 1000) fh iaddr
      foreverWith (atomically $ readTVar ctrl) $ do
        key <- takeMVar queue
        unless (L.null key) $ do
          ok <- sndTimeout' 0 fh [key]
          unless ok $ do
            warning syslog $ "dealer: can't schedule, dropping msg"
            mjob <- readJob dealer key
            unless (isJust mjob) $ reply (fromJust mjob) Nothing
  forkSupervised syslog (atomically $ readTVar ctrl) "dealer/pool" $ do
    let (db, readFunc) = endpoint cfg
    updatePool pool =<< fmap ((:[]) . sort) (readFunc db)
    sleep 1
  return $ ClientFH (dealer, pool, ctrl)
    where
      aliveCheck ctrl0 ctrl1 = atomically $ liftM2 (&&) (readTVar ctrl0) (readTVar ctrl1)

      destroyWorker _ (ctrl, caps, sem) = do
        warning syslog $
          printf "dropping zmq.dealer/worker"
        atomically $ writeTVar ctrl False
        replicateM_ caps (waitQSem sem)

      createWorker ctrl0 dealer iaddr oaddr = do
        warning syslog $
          printf "creating zmq.dealer/worker"
        ctrl <- newTVarIO True
        sem  <- newQSem 0
        if (null oaddr)
          then return (ctrl, 0, sem)
          else do
            replicateM_ (capabilities cfg) $
              forkFinally (dealerLoop ctx dealer (aliveCheck ctrl0 ctrl) iaddr oaddr) (\_ -> signalQSem sem)
            return (ctrl, capabilities cfg, sem)

destroy :: ClientFH -> IO ()
destroy (ClientFH (dealer, pool, ctrl)) = do
  deletePool pool
  atomically $ writeTVar ctrl False
  putMVar (queue dealer) L.empty
  atomically $ readTVar ctrl >>= flip unless retry
