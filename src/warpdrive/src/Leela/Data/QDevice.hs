{-# LANGUAGE TupleSections     #-}
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

module Leela.Data.QDevice
       ( Limit
       , Device ()
       , Control ()
       , HasControl (..)
       , open
       , copy
       , close
       , openIO
       , closed
       , control
       , closeIO
       , devnull
       , devread
       , closedIO
       , devwrite
       , devreadIO
       , notClosed
       , devwriteIO
       , trydevread
       , notClosedIO
       , withControl
       , addFinalizer
       , runFinalizer
       ) where

import Control.Monad
import Leela.Helpers
import Control.Exception
import Leela.Data.Excepts
import Control.Concurrent.STM

type Limit = Int

newtype Control = Control (TVar Bool, TVar [Finalizer])

data Device a = Device Control (TBQueue a)

class HasControl a where
    ctrlof :: a -> Control

type Finalizer = IO ()

control :: STM Control
control = do
  tvar0 <- newTVar False
  tvar1 <- newTVar []
  return (Control (tvar0, tvar1))

addFinalizer :: (HasControl ctrl) => ctrl -> Finalizer -> IO ()
addFinalizer target fin = do
  let (Control (_, tvar)) = ctrlof target
  atomically $ do
    current <- readTVar tvar
    writeTVar tvar (fin : current)

devnull :: STM (Device a)
devnull = do
  ctrl <- control
  fmap (Device ctrl) (newTBQueue 0)

withControl :: (Control -> IO a) -> IO ()
withControl io = mask $ \restore -> do
  ctrl <- atomically control
  _    <- restore (io ctrl) `onException` closeIO ctrl
  closeIO ctrl

closed :: HasControl ctrl => ctrl -> STM Bool
closed box = let Control ctrl = ctrlof box
             in readTVar (fst ctrl)

copy :: Device a -> (a -> Maybe b) -> Device b -> IO ()
copy src f dst = do
  mv <- devreadIO src
  case (join $ fmap (f . fst) mv) of
    Nothing -> return ()
    Just v  -> devwriteIO dst v >> copy src f dst

notClosed :: HasControl ctrl => ctrl -> STM Bool
notClosed = fmap not . closed

notClosedIO :: HasControl ctrl => ctrl -> IO Bool
notClosedIO = atomically . notClosed

closedIO :: HasControl ctrl => ctrl -> IO Bool
closedIO box = let Control ctrl = ctrlof box
               in readTVarIO (fst ctrl)

open :: HasControl ctrl => ctrl -> Limit -> STM (Device a)
open ctrl l = fmap (Device (ctrlof ctrl)) (newTBQueue (max l 1))

openIO :: HasControl ctrl => ctrl -> Limit -> IO (Device a)
openIO ctrl l = atomically $ open ctrl l

close :: HasControl ctrl => ctrl -> STM ()
close ctrl = let Control tvar = ctrlof ctrl
             in writeTVar (fst tvar) True

runFinalizer :: (HasControl ctrl) => ctrl -> IO ()
runFinalizer target = do
  fins <- atomically $ do
    current <- readTVar tmvar
    writeTVar tmvar []
    return current
  mapM_ (`catch` ignore) fins
    where
      Control (_, tmvar) = ctrlof target

closeIO :: HasControl ctrl => ctrl -> IO ()
closeIO ctrl = do
  atomically (close ctrl)
  runFinalizer ctrl `catch` ignore

select :: Device a -> STM (Bool, TBQueue a)
select (Device (Control (ctrl, _)) q) = fmap (, q) (readTVar ctrl)

devwrite :: Device a -> a -> STM ()
devwrite dev v = do
  (notok, q) <- select dev
  when notok (throwSTM (BadDeviceExcept (Just "QDevice/devwrite: device is closed")))
  writeTBQueue q v

devwriteIO :: Device a -> a -> IO ()
devwriteIO dev = atomically . devwrite dev

trydevread :: Device a -> STM (Maybe a)
trydevread dev = fmap snd (select dev) >>= tryReadTBQueue

devread :: Device a -> STM (Maybe (a, Bool))
devread dev = do
  (notok, q) <- select dev
  if notok
    then do
      v <- tryReadTBQueue q
      e <- isEmptyTBQueue q
      return (fmap (, not e) v)
    else do
      v <- readTBQueue q
      return (Just (v, True))

devreadIO :: Device a -> IO (Maybe (a, Bool))
devreadIO = atomically . devread

instance HasControl Control where

    ctrlof = id

instance HasControl (Device a) where

    ctrlof (Device ctrl _) = ctrl
