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

module Leela.Data.Timeout
       ( Handle ()
       , TimeoutInUs
       , TimeoutManager ()
       , timeoutManager
       , open
       , creat
       , touch
       , purge
       , withHandle
       ) where

import GHC.Event
import Data.IORef
import Control.Monad
import Control.Exception

type TimeoutInUs = Int

type Finalizer = IO ()

data Handle = Handle Int TimeoutKey (IORef (IO ()))

data TimeoutManager = TimeoutManager (IORef Int)

timeoutManager :: IO TimeoutManager
timeoutManager = liftM TimeoutManager (newIORef 0)

open :: TimeoutManager -> TimeoutInUs -> Finalizer -> IO (Int, Handle)
open (TimeoutManager ref) timeout fin = do
  tm     <- getSystemTimerManager
  active <- atomicModifyIORef' ref (\x -> (x + 1, x + 1))
  ioref  <- newIORef (atomicModifyIORef' ref (\x -> (x - 1, ())))
  cookie <- registerTimeout tm timeout (executeOnce ioref >> fin)
  return (active, Handle timeout cookie ioref)

creat :: TimeoutManager -> TimeoutInUs -> Finalizer -> IO ()
creat _ timeout fin = do
  tm <- getSystemTimerManager
  void $ registerTimeout tm timeout fin

touch :: Handle -> IO ()
touch (Handle timeout cookie _) = do
  tm <- getSystemTimerManager
  updateTimeout tm cookie timeout

purge :: Handle -> IO ()
purge (Handle _ cookie ioref) = do
  tm <- getSystemTimerManager
  unregisterTimeout tm cookie
  executeOnce ioref

executeOnce :: IORef (IO ()) -> IO ()
executeOnce ioref = do
  join $ atomicModifyIORef' ioref (\io -> (return (), io))

withHandle :: TimeoutManager -> Int -> Finalizer -> IO a -> IO a
withHandle tm timeout fin action = bracket (open tm timeout fin)
                                           (purge . snd)
                                           (const $ action)
