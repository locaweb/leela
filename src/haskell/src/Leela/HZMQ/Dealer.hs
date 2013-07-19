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
       ( Cfg (..)
       , Pool
       , defaultCfg
       , create
       , request
       ) where

import           Data.Int
import           Data.Maybe
import           System.ZMQ3
import           Leela.Logger
import           Control.Monad
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Excepts
import           Leela.HZMQ.ZHelpers
import           Control.Concurrent.STM

data Cfg = Cfg { timeout      :: Int64
               , queueSize    :: Int
               , capabilities :: Int
               }

defaultCfg :: Cfg
defaultCfg = Cfg 5 32 8

data Job = Job { jmsg :: [B.ByteString]
               , slot :: TMVar (Maybe [B.ByteString])
               }

data Pool = Pool { readTimeout :: Int64
                 , queue       :: TBQueue Job
                 }

enqueue :: Pool -> [B.ByteString] -> IO (TMVar (Maybe [B.ByteString]))
enqueue pool a = do
  mvar <- newEmptyTMVarIO
  atomically (writeTBQueue (queue pool) (Job a mvar))
  return mvar

dequeue :: Pool -> IO Job
dequeue pool = atomically (readTBQueue (queue pool))

request :: Pool -> [B.ByteString] -> IO (Maybe [B.ByteString])
request pool a = enqueue pool a >>= atomically . takeTMVar

notify :: TMVar (Maybe [B.ByteString]) -> Maybe [B.ByteString] -> IO ()
notify mvar msg = atomically (putTMVar mvar msg)

logresult :: Job -> Maybe SomeException -> IO ()
logresult _ _ = return ()

worker :: Pool -> Context -> String -> IO ()
worker pool ctx endpoint = do
  withSocket ctx Req $ \fh -> setup fh >> workLoop fh
    where setup fh = do
            connect fh endpoint
            configure fh

          workLoop fh = do
            job  <- dequeue pool
            mres <- try (sendAll fh (jmsg job))
            case mres of
              Left e  -> do
                logresult job (Just e)
                notify (slot job) Nothing
              Right _ -> do
                mresult <- recvTimeout (readTimeout pool) fh
                notify (slot job) mresult
                case mresult of
                  Nothing  ->
                    logresult job (Just $ SomeException SystemExcept)
                  Just res -> do
                    logresult job Nothing
                    workLoop fh

forkSupervised :: IO () -> IO ()
forkSupervised io = forkIO (supervise "dealer.worker" io) >> return ()

forkWorker :: Pool -> Context -> String -> IO ()
forkWorker pool ctx endpoint = forkSupervised (worker pool ctx endpoint)

create :: String -> Cfg -> Context -> [String] -> IO Pool
create name cfg ctx endpoints = do
  lnotice HZMQ $
    printf "creating zmq.dealer: %s [timeout: %d; qsize: %d; capabilities: %d; endpoint: %s]"
           name
           (timeout cfg * 1000)
           (queueSize cfg)
           (capabilities cfg)
           (show endpoints)
  pool <- fmap (Pool (timeout cfg)) (newTBQueueIO (queueSize cfg))
  mapM_ (replicateM_ (capabilities cfg) . forkWorker pool ctx) endpoints
  return pool
