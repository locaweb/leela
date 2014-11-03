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
       , stop
       , timeout
       , destroy
       , open
       , touch
       , purge
       , withHandle
       ) where

import Data.IORef
import Control.Monad
import Leela.Helpers
import Control.Exception
import Control.Concurrent

type Finalizer = IO ()

data State = Run Int
           | Cancel
           deriving (Eq)

data ReaperState = Dead
                 | Inactive
                 | Active
                 deriving (Eq)

data Handle = Handle Int (IORef State) Finalizer

data Manager = Manager Int (IORef ReaperState) (IORef [Handle])

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000 * 1000)

timeout :: Int -> IO Manager
timeout step = do
  st  <- newIORef Inactive
  ref <- newIORef []
  return (Manager step st ref)

forkReaper :: Manager -> IO ()
forkReaper (Manager _ s ref) = do
  v <- atomicModifyIORef' s update
  when (v == Inactive) $
    void $ forkFinally (foreverWith continue (sleep 1 >> reaperStep ref)) (\_ -> reaperCleanup ref)
    where
      update Inactive = (Active, Inactive)
      update x        = (x, x)

      continue = fmap (== Active) (readIORef s)

stop :: Manager -> IO ()
stop (Manager _ state _) =
  writeIORef state Dead

destroy :: Manager -> IO ()
destroy (Manager _ st ref) = do
  writeIORef ref []
  writeIORef st Dead

inspectHandle :: Handle-> IO (Maybe Finalizer)
inspectHandle (Handle _ ref fin) = atomicModifyIORef' ref update
    where
      update (Run x)
        | x <= 0     = (Cancel, Just fin)
        | otherwise  = (Run (x - 1), Nothing)
      update Cancel  = (Cancel, Just (return ()))

inspectList :: [Handle] -> [Handle] -> IO [Handle]
inspectList acc []       = return acc
inspectList acc (x : xs) = do
  term <- inspectHandle x
  case term of
    Just fin -> fin `catch` ignore >> inspectList acc xs
    Nothing  -> inspectList (x : acc) xs

killAll :: [Handle] -> IO ()
killAll []                  = return ()
killAll (Handle _ ref fin : xs) = do
  s <- readIORef ref
  when (s /= Cancel) (fin `catch` ignore)
  killAll xs

reaperStep :: IORef [Handle] -> IO ()
reaperStep ref = do
  xs <- atomicModifyIORef' ref (\ys -> ([], ys))
  ys <- inspectList [] xs
  atomicModifyIORef' ref (\zs -> (ys ++ zs, ()))

reaperCleanup :: IORef [Handle] -> IO ()
reaperCleanup ref =
  atomicModifyIORef' ref (\xs -> ([], xs)) >>= killAll

open :: Manager -> Finalizer -> IO Handle
open tm@(Manager ttl _ ref) fin = do
  forkReaper tm
  state <- newIORef (Run ttl)
  let h = Handle ttl state fin
  atomicModifyIORef' ref (\xs -> (h : xs, h))

touch :: Handle -> IO ()
touch (Handle ttl ref _) = atomicModifyIORef' ref update
    where
      update (Run _) = (Run ttl, ())
      update x       = (x, ())

purge :: Handle -> IO ()
purge (Handle _ ref _) = writeIORef ref Cancel

withHandle :: Manager -> Finalizer -> IO a -> IO a
withHandle tm fin action = bracket (open tm fin) purge (const action)
