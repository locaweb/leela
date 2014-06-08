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
       ( Client
       , ClientConf (..)
       , create
       , request
       ) where

import qualified Data.Map as M
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
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import           Control.Concurrent.STM

data Job = Job { jmsg  :: [B.ByteString]
               , slot  :: MVar (Maybe [B.ByteString])
               }

data ClientConf a = ClientConf { backlog      :: Int
                               , endpoint     :: (a, a -> IO [Endpoint])
                               , capabilities :: Int
                               }

data Client = Client { logger  :: Logger
                     , queue   :: MVar B.ByteString
                     , state   :: TVar (M.Map B.ByteString Job)
                     , counter :: Counter Word64
                     }

request :: Int -> Client -> [B.ByteString] -> IO (Maybe [B.ByteString])
request timeout dealer a = mask $ \restore -> do
  shmem  <- newEmptyMVar
  tick   <- fmap encode $ next (counter dealer)
  createFH shmem tick
  putMVar (queue dealer) tick
  mvalue <- fmap join (restore (T.timeout (timeout * 1000) (takeMVar shmem)) `onException` dropFH tick)
  dropFH tick
  return mvalue
    where
      createFH shmem tick = atomically $ do
        m <- readTVar (state dealer)
        writeTVar (state dealer) (M.insert tick (Job a shmem) m)

      dropFH tick = atomically $ do
        m <- readTVar (state dealer)
        writeTVar (state dealer) (M.delete tick m)

readJob :: Client -> B.ByteString -> IO (Maybe Job)
readJob dealer tick = atomically $ fmap (M.lookup tick) (readTVar (state dealer))

dealerLoop :: Context -> Client -> IO Bool -> StrEndpoint -> [Endpoint] -> IO ()
dealerLoop ctx dealer cont iaddr oaddr = do
  withSocket ctx Dealer $ \ofh -> do
    mapM_ (configAndConnect ofh . dumpEndpointStr) oaddr
    withSocket ctx Pull $ \ifh -> do
      configAndConnect ifh iaddr
      foreverWith cont $ go ifh ofh
        where
          go ifh ofh = do
            [ev0, ev1] <- poll 1000 [ Sock ifh [In] Nothing
                                    , Sock ofh [In] Nothing
                                    ]
            unless (null ev0) $ do
              key  <- receive ifh
              mjob <- readJob dealer key
              when (isJust mjob) $ do
                ok <- sndTimeout 10 ofh (key : B.empty : (jmsg $ fromJust mjob))
                unless ok (putMVar (slot $ fromJust mjob) Nothing)

            unless (null ev1) $ do
              (key, (_:reply)) <- fmap (break B.null) (receiveMulti ofh)
              mmsg             <- readJob dealer (last key)
              when (isJust mmsg) (putMVar (slot $ fromJust mmsg) (Just reply))

create :: Logger -> ClientConf a -> Context -> IO (Client, Pool [Endpoint] (TVar Bool))
create syslog cfg ctx = do
  notice syslog (printf "creating zmq.dealer [capabilities %d]" (capabilities cfg))
  iaddr  <- fmap (printf "inproc://%s" . show) nextRandom :: IO String
  queue  <- newEmptyMVar
  dealer <- liftM2 (Client syslog queue) (newTVarIO M.empty) newCounter
  pool   <- createPool (createWorker dealer iaddr) destroyWorker
  forkSupervised_ syslog "dealer/pool" $ do
    let (db, readFunc) = endpoint cfg
    updatePool pool =<< fmap ((:[]) . sort) (readFunc db)
    sleep 1
  forkSupervised_ syslog "dealor/router" $ withSocket ctx Push $ \fh -> do
    configAndBind fh iaddr
    forever $ do
      msg <- takeMVar queue
      send fh [] msg
  return (dealer, pool)
    where
      aliveCheck ctrl = atomically $ readTVar ctrl

      destroyWorker _ ctrl = do
        warning syslog $
          printf "dropping zmq.dealer/worker"
        atomically $ writeTVar ctrl False

      createWorker dealer iaddr oaddr = do
        warning syslog $
          printf "creating zmq.dealer/worker"
        ctrl <- newTVarIO True
        unless (null oaddr) $
          replicateM_ (capabilities cfg) $
            forkIO (dealerLoop ctx dealer (aliveCheck ctrl) iaddr oaddr)
        return ctrl
