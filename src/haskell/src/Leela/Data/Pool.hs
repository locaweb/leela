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

module Leela.Data.Pool
       ( Pool
       , createPool
       , updatePool
       , deletePool
       , use
       ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad
import           Control.Exception (bracket)
import           Leela.Data.Excepts
import           Control.Concurrent.STM

data Pool a b = Pool { state  :: TVar (M.Map a b)
                     , using  :: TVar (M.Map a Int)
                     , create :: a -> IO b
                     , delete :: a -> b -> IO ()
                     }

createPool :: (a -> IO b) -> (a -> b -> IO ()) -> IO (Pool a b)
createPool add del = do
  m0 <- newTVarIO M.empty
  m1 <- newTVarIO M.empty
  return $ Pool m0 m1 add del

updatePool :: (Ord a) => Pool a b -> [a] -> IO ()
updatePool pool lcluster = do
  (dead, new) <- do
    current <- fmap M.keysSet (readTVarIO (state pool))
    let cluster = S.fromList lcluster
    return (S.toList $ current `S.difference` cluster, S.toList $ cluster `S.difference` current)
  mapM_ (kill pool) dead
  mapM_ (open pool) new

deletePool :: Pool a b -> IO ()
deletePool pool = do
  mvalue <- atomically $ do
    m <- readTVar (state pool)
    if (M.null m)
      then let (value, newState) = M.deleteFindMin m
           in writeTVar (state pool) newState >> return (Just value)
      else return Nothing
  case mvalue of
    Nothing     -> return ()
    Just (k, b) -> do
      delete pool k b
      deletePool pool

kill :: (Ord a) => Pool a b -> a -> IO ()
kill pool k = do
  mhandle <- atomically $ do
    m0 <- readTVar (state pool)
    m1 <- readTVar (using pool)
    when (M.findWithDefault 0 k m1 /= 0) retry
    writeTVar (state pool) (M.delete k m0)
    writeTVar (using pool) (M.delete k m1)
    return (M.lookup k m0)
  case mhandle of
    Nothing     -> return ()
    Just handle -> delete pool k handle

open :: (Ord a) => Pool a b -> a -> IO ()
open pool k = do
  handle  <- create pool k
  atomically $ modifyTVar (state pool) (M.insert k handle)

use :: (Ord a) => Pool a b -> ([a] -> a) -> (b -> IO c) -> IO c
use pool select apply = bracket acquire release (apply . snd)
    where
      acquire = atomically $ do
        m <- readTVar (state pool)
        let a = select (M.keys m)
        modifyTVar' (using pool) (M.insertWith (+) a 1)
        case (M.lookup a m) of
          Just b -> return (a, b)
          _      -> throwSTM SystemExcept

      release (a, _) = atomically $
        modifyTVar' (using pool) (M.insertWith (+) a (-1))
