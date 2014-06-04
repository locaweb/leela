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
       ( Dealer
       , DealerConf (..)
       , create
       , request
       , destroy
       ) where

import           Data.List (sort)
import           Data.Maybe
import           System.ZMQ4 hiding (Dealer, backlog)
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import qualified Data.ByteString as B
import           Leela.Data.Pool
import           Control.Exception
import           Control.Concurrent
import           Data.List.NonEmpty (fromList)
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import           Control.Concurrent.STM

data Job = Job { jmsg  :: [B.ByteString]
               , slot  :: MVar (Maybe [B.ByteString])
               }

data DealerConf a = DealerConf { timeout      :: Int
                               , backlog      :: Int
                               , endpoint     :: (a, a -> IO [Endpoint])
                               , capabilities :: Int
                               }

newtype Dealer = Dealer (Logger, TBQueue (Maybe Job), Pool [Endpoint] (TVar Bool))

enqueue :: TBQueue (Maybe Job) -> [B.ByteString] -> IO (MVar (Maybe [B.ByteString]))
enqueue queue a = do
  mvar <- newEmptyMVar
  atomically $ writeTBQueue queue (Just $ Job a mvar)
  return mvar

request :: Dealer -> [B.ByteString] -> IO (Maybe [B.ByteString])
request (Dealer (_, queue, _)) a =
  enqueue queue a >>= takeMVar

notify :: MVar (Maybe [B.ByteString]) -> Maybe [B.ByteString] -> IO ()
notify mvar mmsg = putMVar mvar mmsg

execWorker :: Logger -> DealerConf a -> Context -> IO (Maybe Job) -> [Endpoint] -> IO ()
execWorker syslog cfg ctx dequeue addr = withSocket ctx Req $ \fh -> do
  mapM_ (configAndConnect fh . dumpEndpointStr) addr
  workLoop fh
    where
      timeout64 = fromIntegral (timeout cfg)

      workLoop fh = do
        mjob <- dequeue
        case mjob of
          Nothing  -> info syslog $ printf "worker is quitting: %s" (show addr)
          Just job -> do
            mres <- try (sendMulti fh (fromList $ jmsg job))
            case mres of
              Left e  -> do
                ignore e
                notify (slot job) Nothing
              Right _ -> do
                mresult <- recvTimeout timeout64 fh
                notify (slot job) mresult
                when (isJust mresult) (workLoop fh)

create :: Logger -> DealerConf a -> Context -> IO Dealer
create syslog cfg ctx = do
  notice syslog "creating zmq.dealer"
  queue <- newTBQueueIO (backlog cfg)
  pool  <- createPool (createWorker queue) destroyWorker
  forkSupervised_ syslog "dealer/pool" $ do
    let (db, readFunc) = endpoint cfg
    updatePool pool =<< fmap ((:[]) . sort) (readFunc db)
    sleep 1
  return (Dealer (syslog, queue, pool))
    where
      dequeue ctrl queue = atomically $ do
        ok <- readTVar ctrl
        if ok
          then readTBQueue queue
          else return Nothing

      aliveCheck ctrl = atomically $ readTVar ctrl

      destroyWorker addr ctrl = do
        warning syslog $
          printf "dropping zmq.dealer/worker: endpoint=%s" (show addr)
        atomically $ writeTVar ctrl False

      createWorker queue addr = do
        ctrl <- newTVarIO True
        warning syslog $
          printf "creating zmq.dealer/worker: endpoint=%s, capabilities=%d, timeout=%d" (show addr) (capabilities cfg) (timeout cfg)
        replicateM_ (capabilities cfg) $
          forkSupervised
            syslog
            (aliveCheck ctrl)
            ("dealer.worker/" ++ (show addr))
            (execWorker syslog cfg ctx (dequeue ctrl queue) addr)
        return ctrl

destroy :: Dealer -> IO ()
destroy (Dealer (_, _, pool)) = deletePool pool

