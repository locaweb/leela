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
import           Leela.Data.Time
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
               , slot  :: MVar (Maybe [B.ByteString])
               }

data DealerConf a = DealerConf { timeout      :: Int
                               , backlog      :: Int
                               , endpoint     :: (a, a -> IO [Endpoint])
                               , capabilities :: Int
                               }

newtype Dealer = Dealer (Logger, Device Job, Pool [Endpoint] (TMVar ()))

enqueue :: Device Job -> [B.ByteString] -> IO (MVar (Maybe [B.ByteString]))
enqueue queue a = do
  time <- now
  mvar <- newEmptyMVar
  devwriteIO queue (Job time a mvar)
  return mvar

request :: Dealer -> [B.ByteString] -> IO (Maybe [B.ByteString])
request (Dealer (_, queue, _)) a =
  enqueue queue a >>= takeMVar

notify :: MVar (Maybe [B.ByteString]) -> Maybe [B.ByteString] -> IO ()
notify mvar mmsg = putMVar mvar mmsg

logresult :: Logger -> Job -> Maybe SomeException -> IO ()
logresult syslog job me = do
  elapsed <- fmap (`diff` (jtime job)) now
  info syslog $ printf "%s (%.4fms)" (failOrSucc me) (1000 * elapsed)
    where
      failOrSucc :: Maybe SomeException -> String
      failOrSucc Nothing  = "DEALER.ok"
      failOrSucc (Just e) = printf "DEALER.fail[%s]" (show e)

execWorker :: Logger -> DealerConf a -> Context -> IO (Maybe Job) -> [Endpoint] -> IO ()
execWorker syslog cfg ctx dequeue addr = withSocket ctx Req $ \fh -> do
  mapM_ (connect fh . dumpEndpointStr) addr
  configure fh
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
                logresult syslog job (Just e)
                notify (slot job) Nothing
              Right _ -> do
                mresult <- recvTimeout timeout64 fh
                notify (slot job) mresult
                logresult syslog job (maybe (Just $ SomeException TimeoutExcept) (const Nothing) mresult)
                when (isJust mresult) (workLoop fh)

create :: Logger -> DealerConf a -> Context -> Control -> IO Dealer
create syslog cfg ctx ctrl = do
  notice syslog "creating zmq.dealer"
  queue <- openIO ctrl (backlog cfg)
  pool  <- createPool (createWorker queue) destroyWorker
  _     <- forkSupervised syslog (notClosedIO ctrl) "dealer/watcher" $ do
    let (db, readFunc) = endpoint cfg
    updatePool pool =<< fmap ((:[]) . sort) (readFunc db)
    threadDelay 1000000
  return (Dealer (syslog, queue, pool))
    where
      dequeue ctl queue = atomically $ do
        ok <- isEmptyTMVar ctl
        if ok
          then devread queue
          else return Nothing

      aliveCheck ctl = atomically $
        liftM2 (&&) (isEmptyTMVar ctl) (notClosed ctrl)

      destroyWorker addr ctl = do
        warning syslog $
          printf "dropping zmq.dealer/worker: endpoint=%s" (show addr)
        void $ atomically $ tryPutTMVar ctl ()

      createWorker queue addr = do
        ctl <- newEmptyTMVarIO
        warning syslog $
          printf "creating zmq.dealer/worker: endpoint=%s, capabilities=%d, timeout=%d" (show addr) (capabilities cfg) (timeout cfg)
        replicateM_ (capabilities cfg) $
          forkSupervised
            syslog
            (aliveCheck ctl)
            ("dealer.worker/" ++ (show addr))
            (execWorker syslog cfg ctx (dequeue ctl queue) addr)
        return ctl

destroy :: Dealer -> IO ()
destroy (Dealer (_, _, pool)) = deletePool pool

