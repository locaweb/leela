{-# LANGUAGE TupleSections #-}

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
       , rr
       , use
       , useAll
       ) where

import qualified Data.Set as S
import           Control.Monad
import qualified Data.Map.Strict as M
import           Control.Exception (bracket, throwIO)
import           Leela.Data.Excepts
import           Control.Concurrent.STM

data Pool a b = Pool { state  :: TVar (M.Map a b)
                     , using  :: TVar (M.Map a Int)
                     , rridx  :: TVar (Int, Int)
                     , create :: a -> IO b
                     , delete :: a -> b -> IO ()
                     }

createPool :: (a -> IO b) -> (a -> b -> IO ()) -> IO (Pool a b)
createPool add del = do
  m0 <- newTVarIO M.empty
  m1 <- newTVarIO M.empty
  m2 <- newTVarIO (0, 0)
  return $ Pool m0 m1 m2 add del

updatePool :: (Ord a) => Pool a b -> [a] -> IO ()
updatePool pool lcluster = do
  (dead, new) <- do
    current <- fmap M.keysSet (readTVarIO (state pool))
    let cluster = S.fromList lcluster
    return (S.toList $ current `S.difference` cluster, S.toList $ cluster `S.difference` current)
  mapM_ (open pool) new
  mapM_ (kill pool) dead

deletePool :: Pool a b -> IO ()
deletePool pool = do
  items <- atomically $ do
    m <- readTVar (state pool)
    writeTVar (state pool) M.empty
    writeTVar (rridx pool) (0, 0)
    return (M.toList m)
  mapM_ (uncurry $ delete pool) items

updateState :: Pool a b -> (M.Map a b -> (c, M.Map a b)) -> STM c
updateState pool f = do
  (c, m) <- fmap f (readTVar $ state pool)
  let sz = M.size m
  writeTVar (state pool) m
  modifyTVar' (rridx pool) (\(ix, _) -> (ix, sz))
  return c

kill :: (Ord a) => Pool a b -> a -> IO ()
kill pool k = do
  mhandle <- atomically $
    updateState pool (\m -> (M.lookup k m, M.delete k m))
  _       <- atomically $ do
    m1 <- readTVar (using pool)
    when (M.findWithDefault 0 k m1 /= 0) retry
    writeTVar (using pool) (M.delete k m1)
  case mhandle of
    Nothing     -> return ()
    Just handle -> delete pool k handle

open :: (Ord a) => Pool a b -> a -> IO ()
open pool k = do
  handle <- create pool k
  atomically $
    updateState pool (\m -> ((), M.insert k handle m))

use :: (Ord a) => Pool a b -> ([a] -> a) -> (b -> IO c) -> IO c
use pool select apply = bracket acquire release (apply . snd)
    where
      acquire = do
        m <- atomically $ readTVar (state pool)
        let a = select (M.keys m)
        case (M.lookup a m) of
          Just b -> atomically $ do
            modifyTVar' (using pool) (M.insertWith (+) a 1)
            return (a, b)
          _      -> throwIO (SystemExcept (Just "Pool/use: can't find pool entry"))

      release (a, _) = atomically $
        modifyTVar' (using pool) (M.insertWith (-) a 1)

rr :: (Ord a) => Pool a b -> (b -> IO c) -> IO c
rr pool apply = bracket acquire release (apply . snd)
    where
      acquire = do
        (at, m) <- atomically $ do
          (ix, sz) <- readTVar $ rridx pool
          let next = (ix + 1) `mod` sz
          writeTVar (rridx pool) (next, sz)
          fmap (next, ) (readTVar $ state pool)
        let (a, b) = M.elemAt at m
        atomically $ do
          modifyTVar' (using pool) (M.insertWith (+) a 1)
          return (a, b)

      release (a, _) = atomically $
        modifyTVar' (using pool) (M.insertWith (-) a 1)

useAll :: (Ord a) => Pool a b -> ([b] -> IO c) -> IO c
useAll pool apply = bracket acquire release (apply . snd)
    where
      acquire = do
        m <- atomically $ readTVar (state pool)
        let (as, vs) = unzip (M.assocs m)
        atomically $ do
          mapM_ (\a -> modifyTVar' (using pool) (M.insertWith (+) a 1)) as
          return (as, vs)

      release (as, _) = atomically $
        mapM_ (\a -> modifyTVar' (using pool) (M.insertWith (-) a 1)) as
