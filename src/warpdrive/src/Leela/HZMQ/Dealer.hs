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
import           System.ZMQ4
import           Data.UUID.V4
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Data.Serialize
import qualified Data.ByteString as B
import           Leela.Data.Pool
import           Leela.Data.Time
import           Control.Exception
import qualified Data.HashMap.Lazy as H
import           Control.Concurrent
import           Leela.Data.Counter
import           Leela.Data.Endpoint
import           Leela.HZMQ.ZHelpers
import qualified Data.ByteString.Lazy as L
import           Control.Concurrent.STM

data Job = Job { time  :: Time
               , jmsg  :: [L.ByteString]
               , slot  :: MVar (Maybe [B.ByteString])
               }

data ClientConf a = ClientConf { endpoint     :: (a, a -> IO [Endpoint])
                               , capabilities :: Int
                               }

data Client = Client { logger  :: Logger
                     , queue   :: MVar L.ByteString
                     , state   :: TVar (H.HashMap L.ByteString Job)
                     , counter :: Counter Word64
                     }

newtype ClientFH = ClientFH (Client, Pool [Endpoint] (TVar Bool, Int, QSem), TVar Bool)

spinTimeout :: Int -> IO (Maybe a) -> IO (Maybe a)
spinTimeout timeout io
  | timeout > 0 = io >>= retryOnNothing
  | otherwise   = return Nothing
    where
      retryOnNothing Nothing  = threadDelay 1000 >> spinTimeout (timeout - 1) io
      retryOnNothing v        = return v

request :: Int -> ClientFH -> [L.ByteString] -> IO (Maybe [B.ByteString])
request timeout (ClientFH (dealer, _, _)) a = mask $ \restore -> do
  shmem <- newEmptyMVar
  tick  <- fmap encodeLazy $ next (counter dealer)
  createFH shmem tick
  putMVar (queue dealer) tick
  fmap join (restore (spinTimeout timeout (tryTakeMVar shmem)) `finally` (dropFH tick))
    where
      createFH shmem tick = snapshot >>= \t -> atomically $ do
       m <- readTVar (state dealer)
       writeTVar (state dealer) (H.insert tick (Job t a shmem) m)

      dropFH tick = atomically $ do
        m <- readTVar (state dealer)
        writeTVar (state dealer) (H.delete tick m)

readJob :: Client -> L.ByteString -> IO (Maybe Job)
readJob dealer tick = fmap (H.lookup tick) (readTVarIO (state dealer))

reply :: Job -> Maybe [B.ByteString] -> IO ()
reply job ans = putMVar (slot job) ans

dealerLoop :: Context -> Client -> TVar Bool -> StrEndpoint -> [Endpoint] -> IO ()
dealerLoop ctx dealer ctrl iaddr oaddr = do
  withSocket ctx Dealer $ \ofh -> do
    setHWM (1000, 1000) ofh
    mapM_ (configAndConnect ofh . dumpEndpointStr) oaddr
    withSocket ctx Pull $ \ifh -> do
      setHWM (1000, 1000) ifh
      configAndConnect ifh iaddr
      warning (logger dealer) (printf "%s: creating dealer/dealer" iaddr)
      foreverWith (readTVarIO ctrl) (go ifh ofh)
      warning (logger dealer) (printf "%s: draining dealer/dealer" iaddr)
      drain ifh ofh
      warning (logger dealer) (printf "%s: dealer/dealer has quit" iaddr)
        where
          drain ifh ofh = do
            [ev0, ev1] <- poll 5000 [ Sock ifh [In] Nothing, Sock ofh [In] Nothing ]
            unless (null ev0 && null ev1) (go ifh ofh >> drain ifh ofh)

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
              mjob           <- readJob dealer (L.fromStrict $ last key)
              when (isJust mjob) $ do
                let job = fromJust mjob
                t <- fmap (milliseconds . (`diff` (time job))) snapshot
                when (t > 200) (warning (logger dealer) (printf "dealer: request time %s ms" (showDouble t)))
                reply job (Just ans)

routerLoop :: Context -> Client -> TVar Bool  -> StrEndpoint -> IO ()
routerLoop ctx dealer ctrl addr = do
  withSocket ctx Push $ \fh -> do
    setHWM (1000, 1000) fh
    configAndBind fh addr
    warning (logger dealer) (printf "%s: creating dealer/router" addr)
    foreverWith (readTVarIO ctrl) (loop fh)
    warning (logger dealer) (printf "%s: dealer/router has quit" addr)
    where
      loop fh = do
        key <- takeMVar (queue dealer)
        unless (L.null key) $ do
          ok <- sndTimeout' 0 fh [key]
          unless ok $ do
            warning (logger dealer) "dealer: can't schedule, dropping msg"
            mjob <- readJob dealer key
            unless (isJust mjob) $ reply (fromJust mjob) Nothing

create :: Logger -> ClientConf a -> Context -> IO ClientFH
create syslog cfg ctx = do
  notice syslog (printf "creating zmq.dealer [capabilities %d]" (capabilities cfg))
  ctrl   <- newTVarIO True
  queue  <- newEmptyMVar
  dealer <- liftM2 (Client syslog queue) (newTVarIO H.empty) newCounter
  pool   <- createPool (createWorker dealer) destroyWorker
  _      <- forkFinally (foreverWith (readTVarIO ctrl) $ do
    let (db, readFunc) = endpoint cfg
    updatePool pool =<< fmap ((:[]) . sort) (readFunc db)
    sleep 1) (\_ -> atomically $ writeTVar ctrl True)
  return $ ClientFH (dealer, pool, ctrl)
    where
      destroyWorker _ (ctrl, caps, sem) = do
        atomically $ writeTVar ctrl False
        replicateM_ caps (waitQSem sem)
        warning syslog "dropping zmq.dealer/worker"

      createWorker dealer oaddr = do
        warning syslog "creating zmq.dealer/worker"
        sem  <- newQSem 0
        ctrl <- newTVarIO True
        if (null oaddr)
          then return (ctrl, 0, sem)
          else do
            iaddr <- fmap (printf "inproc://dealer-%s" . show) nextRandom :: IO String
            forkFinally (routerLoop ctx dealer ctrl iaddr) (\_ -> signalQSem sem)
            replicateM_ (capabilities cfg) $
              forkFinally (dealerLoop ctx dealer ctrl iaddr oaddr) (\_ -> signalQSem sem)
            return (ctrl, capabilities cfg + 1, sem)

destroy :: ClientFH -> IO ()
destroy (ClientFH (dealer, pool, ctrl)) = do
  atomically (writeTVar ctrl False)
  atomically $ readTVar ctrl >>= flip unless retry
  putMVar (queue dealer) L.empty
  deletePool pool
