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
       , create
       , request
       , destroy
       ) where

import           Data.List (isPrefixOf)
import           Data.Maybe
import           System.ZMQ3 hiding (Dealer, destroy, backlog)
import           Leela.Logger
import           Leela.Config
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

execWorker :: Context -> Int -> IO (Maybe Job) -> Endpoint-> IO ()
execWorker ctx timeout dequeue endpoint = withSocket ctx Req $ \fh -> do
  connect fh (toZmq (error "unknown endpoint") endpoint)
  configure fh
  workLoop fh
    where
      timeout64 = fromIntegral timeout

      workLoop fh = do
        mjob <- dequeue
        case mjob of
          Nothing  -> ldebug HZMQ $ printf "worker is quitting: %s" (show endpoint)
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

create :: Cfg -> Context -> Control -> String -> String -> IO Dealer
create cfg ctx ctrl conf remote = do
  backlog <- fmap (maybe 64 id) (cfgGet asInt cfg conf "backlog")
  lnotice HZMQ $
    printf "creating zmq.dealer: conf: %s, remote: %s, backlog: %d" conf remote backlog
  queue <- openIO ctrl backlog
  pool  <- createPool (createWorker queue) destroyWorker
  void $ forkSupervised (notClosedIO ctrl) "dealer/watcher" $ do
    updatePool pool =<< cfgGetM loadEndpoint cfg (remote `isPrefixOf`) "endpoint"
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

      destroyWorker endpoint ctl = do
        lnotice HZMQ $
          printf "dropping zmq.dealer/worker: endpoint=%s" (show endpoint)
        void $ atomically $ tryPutTMVar ctl ()

      createWorker queue endpoint = do
        ctl          <- newEmptyTMVarIO
        timeout      <- fmap (maybe 10000 id) (cfgGet asInt cfg conf "timeout-in-ms")
        capabilities <- fmap (maybe 8 id) (cfgGet asInt cfg conf "capabilities")
        lnotice HZMQ $
          printf "creating zmq.dealer/worker: endpoint=%s, capabilities=%d, timeout=%d" (show endpoint) capabilities timeout
        replicateM_ capabilities $
          forkSupervised
            (aliveCheck ctl)
            ("dealer.worker/" ++ (show endpoint))
            (execWorker ctx timeout (dequeue ctl queue) endpoint)
        return ctl

destroy :: Dealer -> IO ()
destroy (Dealer (_, pool)) = deletePool pool
