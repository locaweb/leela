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

module Leela.Data.EventTree
       ( Distribution ()
       , makeDistribution
       , stopDistribution
       , startDistribution
       , makeStartDistribution
       , attach
       , detach
       , publish
       , subscribe
       , unsubscribe
       ) where

import Data.List (delete)
import Data.IORef
import Control.Monad
import Leela.DataHelpers
import Control.Concurrent
import Control.Applicative

data Distribution a b = Distribution { dThread :: MVar ThreadId
                                     , dSource :: MVar a
                                     , dSelect :: a -> Maybe b
                                     , dClients :: IORef [MVar b]
                                     }

makeDistribution :: (a -> Maybe b) -> IO (Distribution a b)
makeDistribution select = Distribution <$> newEmptyMVar <*> newEmptyMVar <*> pure select <*> newIORef []

makeStartDistribution :: (a -> Maybe b) -> IO (Distribution a b)
makeStartDistribution select = do
  dist <- makeDistribution select
  startDistribution dist
  return dist

startDistribution :: Distribution a b -> IO ()
startDistribution d = do
  pid <- forkFinally (forever $ do
           a <- takeMVar (dSource d)
           case (dSelect d a) of
             Nothing -> return ()
             Just b  -> distribute b =<< readIORef (dClients d)) (\_ -> void (tryTakeMVar (dSource d)))
  putMVar (dThread d) pid
    where
      distribute v group = do
        let limit     = 1000
            chunks    = chunked limit group
            broadcast = mapM_ (flip putMVar v)
        if (length group < limit)
          then broadcast group
          else do
            wait <- newQSemN 0
            mapM_ (flip forkFinally (\_ -> signalQSemN wait 1) . broadcast) chunks
            waitQSemN wait (length chunks)

stopDistribution :: Distribution a b -> IO ()
stopDistribution d = maybe (return ()) killThread =<< tryTakeMVar (dThread d)

attach :: Distribution a b -> Distribution b c -> IO ()
attach src dst = atomicModifyIORef' (dClients src) (\list -> (dSource dst : list, ()))

detach :: Distribution a b -> Distribution b c -> IO ()
detach src dst = atomicModifyIORef' (dClients src) (\list -> (delete (dSource dst) list, ()))

subscribe :: Distribution a b -> IO (MVar b)
subscribe dist = do
  mvar <- newEmptyMVar
  atomicModifyIORef' (dClients dist) (\list -> (mvar : list, mvar))

unsubscribe :: Distribution a b -> MVar b -> IO ()
unsubscribe dist mvar = atomicModifyIORef' (dClients dist) (\list -> (delete mvar list, ()))

publish :: Distribution a b -> a -> IO ()
publish dist = putMVar (dSource dist)
