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
       , linger
       , openIO
       , closed
       , control
       , devnull
       , devread
       , blkread
       , closeIO
       , closedIO
       , devwrite
       , lingerIO
       , blkreadIO
       , devreadIO
       , notClosed
       , devwriteIO
       , trydevread
       , notClosedIO
       , withControl
       ) where

import Control.Monad
import Control.Exception
import Leela.Data.Excepts
import Control.Concurrent.STM

type Limit = Int

newtype Control = Control (TVar Bool)

data Device a = Device (TVar Bool) (TBQueue a)

class HasControl a where
    ctrlof :: a -> Control

control :: STM Control
control = fmap Control (newTVar False)

devnull :: STM (Device a)
devnull = do
  ctrl <- newTVar True
  fmap (Device ctrl) (newTBQueue 0)

withControl :: (Control -> IO a) -> IO ()
withControl io = mask $ \restore -> do
  ctrl <- atomically control
  _    <- restore (io ctrl) `onException` closeIO ctrl
  closeIO ctrl

closed :: HasControl ctrl => ctrl -> STM Bool
closed box = let Control ctrl = ctrlof box
             in readTVar ctrl

copy :: Device a -> (a -> Maybe b) -> Device b -> IO ()
copy src f dst = do
  mv <- devreadIO src
  case (join $ fmap f mv) of
    Nothing -> return ()
    Just v  -> devwriteIO dst v >> copy src f dst

notClosed :: HasControl ctrl => ctrl -> STM Bool
notClosed = fmap not . closed

notClosedIO :: HasControl ctrl => ctrl -> IO Bool
notClosedIO = atomically . notClosed

closedIO :: HasControl ctrl => ctrl -> IO Bool
closedIO = atomically . closed

open :: HasControl ctrl => ctrl -> Limit -> STM (Device a)
open ctrl0 l = fmap (Device ctrl) (newTBQueue (max l 1))
    where Control ctrl = ctrlof ctrl0

openIO :: HasControl ctrl => ctrl -> Limit -> IO (Device a)
openIO ctrl l = atomically $ open ctrl l

close :: HasControl ctrl => ctrl -> STM ()
close ctrl = let Control tvar = ctrlof ctrl
             in writeTVar tvar True

linger :: HasControl ctrl => ctrl -> STM ()
linger ctrl = let Control tvar = ctrlof ctrl
              in do isClosed <- readTVar tvar
                    unless isClosed retry

lingerIO :: HasControl ctrl => ctrl -> IO ()
lingerIO = atomically . linger

closeIO :: HasControl ctrl => ctrl -> IO ()
closeIO = atomically . close

select :: Device a -> STM (Bool, TBQueue a)
select (Device ctrl q) = fmap (, q) (readTVar ctrl)

devwrite :: Device a -> a -> STM ()
devwrite dev v = do
  (notok, q) <- select dev
  when notok (throwSTM BadDeviceExcept)
  writeTBQueue q v

devwriteIO :: Device a -> a -> IO ()
devwriteIO dev = atomically . devwrite dev

trydevread :: Device a -> STM (Maybe a)
trydevread dev = fmap snd (select dev) >>= tryReadTBQueue

devread :: Device a -> STM (Maybe a)
devread dev = do
  (notok, q) <- select dev
  if notok
    then tryReadTBQueue q
    else fmap Just (readTBQueue q)

devreadIO :: Device a -> IO (Maybe a)
devreadIO = atomically . devread

blkreadIO :: Limit -> Device a -> IO [a]
blkreadIO limit = atomically . blkread limit

blkread :: Limit -> Device a -> STM [a]
blkread limit dev = blkread_ (max 1 limit) []
    where
      blkread_ 0 acc = return acc
      blkread_ l acc = do
        mmsg <- readfunc l
        case mmsg of
          Just msg -> blkread_ (l - 1) (msg : acc)
          Nothing  -> return acc

      readfunc l
          | l == limit = devread dev
          | otherwise  = trydevread dev

instance HasControl Control where

    ctrlof = id

instance HasControl (Device a) where

    ctrlof (Device ctrl _) = Control ctrl
