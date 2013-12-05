-- This file is part of Leela.
--
-- Leela is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Leela is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Leela.  If not, see <http://www.gnu.org/licenses/>.

module Leela.HZMQ.Dealer
       ( Dealer
       , DealerConf (..)
       , create
       , request
       , destroy
       ) where

import           Data.IORef
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

data DealerConf = DealerConf { timeout      :: Int
                             , backlog      :: Int
                             , endpoint     :: IORef [Endpoint]
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
  linfo HZMQ $ printf "%s (%.4fms)" (failOrSucc me) (1000 * toDouble elapsed)
    where
      failOrSucc :: Maybe SomeException -> String
      failOrSucc Nothing  = "DEALER.ok"
      failOrSucc (Just e) = printf "DEALER.fail[%s]" (show e)

execWorker :: DealerConf -> Context -> IO (Maybe Job) -> Endpoint -> IO ()
execWorker cfg ctx dequeue addr = withSocket ctx Req $ \fh -> do
  connect fh (toZmq (error "unknown endpoint") addr)
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

create :: DealerConf -> Context -> Control -> IO Dealer
create cfg ctx ctrl = do
  lnotice HZMQ "creating zmq.dealer"
  queue <- openIO ctrl (backlog cfg)
  pool  <- createPool (createWorker queue) destroyWorker
  void $ forkSupervised (notClosedIO ctrl) "dealer/watcher" $ do
    updatePool pool =<< readIORef (endpoint cfg)
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
