{-# LANGUAGE TupleSections     #-}

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
       ( QDevice ()
       , qnew
       , copy
       , qlink
       , qread
       , qwrite
       , qclose
       , qTryRead
       , qTryWrite
       , qBulkRead
       ) where

import Control.Monad
import Leela.Data.Excepts
import Control.Applicative
import Control.Concurrent.STM

data QDevice a = QDevice Int (TVar Bool) (TBQueue a)

qnew :: Int -> IO (QDevice a)
qnew size = do
  s <- newTVarIO True
  q <- newTBQueueIO size
  return (QDevice size s q)

qlink :: Int -> QDevice a -> IO (QDevice b)
qlink size (QDevice _ s _) = do
  q <- newTBQueueIO size
  return (QDevice size s q)

qclose :: QDevice a -> IO ()
qclose (QDevice _ s _) =
  atomically $ writeTVar s False

qTryWrite :: QDevice a -> a -> IO ()
qTryWrite (QDevice _ _ q) a = atomically $ do
  writeTBQueue q a `orElse` return ()

qwrite :: QDevice a -> a -> IO ()
qwrite (QDevice _ s q) a = atomically $ do
  open <- readTVar s
  unless open (throwSTM $ BadDeviceExcept (Just "qwrite: device is closed"))
  writeTBQueue q a

qread :: QDevice a -> IO (Maybe a)
qread (QDevice _ s q) = atomically $ do
  open <- readTVar s
  if (not open)
    then tryReadTBQueue q
    else Just <$> readTBQueue q

qBulkRead :: QDevice a -> IO [a]
qBulkRead dev@(QDevice size _ q) = do
  ans <- qread dev
  case ans of
    Nothing -> return []
    Just x  -> go size [x]
    where
      go 0 acc = return (reverse acc)
      go n acc = do
        ans <- atomically $ tryReadTBQueue q
        case ans of
          Nothing -> return (reverse acc)
          Just x  -> go (n - 1) (x : acc)

qTryRead :: QDevice a -> IO (Maybe a, Bool)
qTryRead (QDevice _ s q) = atomically $ do
  mval <- tryReadTBQueue q
  case mval of
    Nothing -> (Nothing,) <$> readTVar s
    Just a  -> do
      open <- readTVar s
      if open
        then return (Just a, True)
        else (Just a,) <$> not <$> isEmptyTBQueue q

copy :: (a -> (Bool, Maybe b)) -> QDevice a -> QDevice b -> IO ()
copy f src dst = do
  mv <- qread src
  case (maybe (False, Nothing) f mv) of
    (cont, Nothing) -> when cont (copy f src dst)
    (cont, Just v)  -> do
      qwrite dst v
      when cont (copy f src dst)
