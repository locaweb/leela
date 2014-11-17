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
       , Manager ()
       , timeoutManager
       , open
       , touch
       , purge
       , withHandle
       ) where

import GHC.Event
import Data.IORef
import Control.Monad
import Control.Exception

type Finalizer = IO ()

data Handle = Handle Int TimeoutKey (IORef (IO ()))

data Manager = Manager (IORef Int)

timeoutManager :: IO Manager
timeoutManager = do
  ref <- newIORef 0
  return (Manager ref)

open :: Manager -> (Int -> Bool) -> Int -> Finalizer -> IO (Maybe (Int, Handle))
open (Manager ref) accept timeout fin = do
  let tuple a b = a `seq` (a, b)
  tm     <- getSystemTimerManager
  active <- atomicModifyIORef' ref (\x -> if (accept x) then tuple (x + 1) x else (x, x))
  if (accept active)
    then do
      ioref  <- newIORef (atomicModifyIORef' ref (\x -> tuple (x - 1) ()))
      cookie <- registerTimeout tm timeout (executeOnce ioref >> fin)
      return (Just (active + 1, Handle timeout cookie ioref))
    else return Nothing

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

withHandle :: Manager -> Int -> Finalizer -> IO a -> IO a
withHandle tm timeout fin action = bracket (open tm (const True) timeout fin)
                                           (maybe (return ()) (purge . snd))
                                           (const $ action)
