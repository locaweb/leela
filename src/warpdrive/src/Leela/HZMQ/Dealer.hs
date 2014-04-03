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

import           Data.Maybe
import           System.ZMQ3 hiding (Dealer, destroy, backlog)
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Leela.Data.Pool
import           Leela.Data.Time
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Data.List.NonEmpty (fromList)
import           Leela.Data.QDevice
import           Leela.Data.Excepts
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import           Control.Concurrent.STM

data Job = Job { jtime :: Time
               , jmsg  :: [B.ByteString]
               , slot  :: TMVar (Maybe [B.ByteString])
               }

data DealerConf a = DealerConf { timeout      :: Int
                               , backlog      :: Int
                               , endpoint     :: (a, a -> IO [Endpoint])
                               , capabilities :: Int
                               }

newtype Dealer = Dealer (Device Job, Pool Endpoint (TMVar ()))

enqueue :: Device Job -> [B.ByteString] -> IO (TMVar (Maybe [B.ByteString]))
enqueue queue a = do
  time <- now
  mvar <- newEmptyTMVarIO
  devwriteIO queue (Job time a mvar)
  return mvar

request :: Dealer -> [B.ByteString] -> IO (Maybe [B.ByteString])
request (Dealer (queue, _)) a = enqueue queue a >>= atomically . takeTMVar

notify :: TMVar (Maybe [B.ByteString]) -> Maybe [B.ByteString] -> IO ()
notify mvar mmsg = atomically (putTMVar mvar mmsg)

logresult :: Job -> Maybe SomeException -> IO ()
logresult job me = do
  elapsed <- fmap (`diff` (jtime job)) now
  linfo HZMQ $ printf "%s (%.4fms)" (failOrSucc me) (1000 * elapsed)
    where
      failOrSucc :: Maybe SomeException -> String
      failOrSucc Nothing  = "DEALER.ok"
      failOrSucc (Just e) = printf "DEALER.fail[%s]" (show e)

execWorker :: DealerConf a -> Context -> IO (Maybe Job) -> Endpoint -> IO ()
execWorker cfg ctx dequeue addr = withSocket ctx Req $ \fh -> do
  connect fh (dumpEndpointStr addr)
  configure fh
  workLoop fh
    where
      timeout64 = fromIntegral (timeout cfg)

      workLoop fh = do
        mjob <- dequeue
        case mjob of
          Nothing  -> ldebug HZMQ $ printf "worker is quitting: %s" (show addr)
          Just job -> do
            mres <- try (sendMulti fh (fromList $ jmsg job))
            case mres of
              Left e  -> do
                logresult job (Just e)
                notify (slot job) Nothing
              Right _ -> do
                mresult <- recvTimeout timeout64 fh
                notify (slot job) mresult
                logresult job (maybe (Just $ SomeException TimeoutExcept) (const Nothing) mresult)
                when (isJust mresult) (workLoop fh)

create :: DealerConf a -> Context -> Control -> IO Dealer
create cfg ctx ctrl = do
  lnotice HZMQ "creating zmq.dealer"
  queue <- openIO ctrl (backlog cfg)
  pool  <- createPool (createWorker queue) destroyWorker
  _     <- forkSupervised (notClosedIO ctrl) "dealer/watcher" $ do
    let (db, readFunc) = endpoint cfg
    updatePool pool =<< readFunc db
    threadDelay 1000000
  return (Dealer (queue, pool))
    where
      dequeue ctl queue = atomically $ do
        ok <- isEmptyTMVar ctl
        if ok
          then devread queue
          else return Nothing

      aliveCheck ctl = atomically $
        liftM2 (&&) (isEmptyTMVar ctl) (notClosed ctrl)

      destroyWorker addr ctl = do
        lnotice HZMQ $
          printf "dropping zmq.dealer/worker: endpoint=%s" (show addr)
        void $ atomically $ tryPutTMVar ctl ()

      createWorker queue addr = do
        ctl <- newEmptyTMVarIO
        lnotice HZMQ $
          printf "creating zmq.dealer/worker: endpoint=%s, capabilities=%d, timeout=%d" (show addr) (capabilities cfg) (timeout cfg)
        replicateM_ (capabilities cfg) $
          forkSupervised
            (aliveCheck ctl)
            ("dealer.worker/" ++ (show addr))
            (execWorker cfg ctx (dequeue ctl queue) addr)
        return ctl

destroy :: Dealer -> IO ()
destroy (Dealer (_, pool)) = deletePool pool
