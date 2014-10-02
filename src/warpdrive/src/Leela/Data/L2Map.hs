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

module Leela.Data.L2Map
       ( L2Map ()
       , empty
       , insert
       , lookup
       , delete
       ) where

import qualified Focus as F
import           Prelude hiding (lookup)
import           Data.Hashable
import qualified STMContainers.Map as M
import           Control.Concurrent.STM

newtype L2Map k1 k2 v = L2Map (M.Map (k1, k2) v)

empty :: IO (L2Map k1 k2 v)
empty = fmap L2Map (atomically M.new)

insert :: (Eq k1, Eq k2, Hashable k1, Hashable k2) => k1 -> k2 -> v -> L2Map k1 k2 v -> IO ()
insert k1 k2 value (L2Map m) =
  let k = (k1, k2)
  in atomically $ M.insert value k m

lookup :: (Eq k1, Eq k2, Hashable k1, Hashable k2) => k1 -> k2 -> L2Map k1 k2 v -> IO (Maybe v)
lookup k1 k2 (L2Map m) =
  let k = (k1, k2)
  in atomically $ M.lookup k m

delete :: (Eq k1, Eq k2, Hashable k1, Hashable k2) => k1 -> k2 -> L2Map k1 k2 v -> IO (Maybe v)
delete k1 k2 (L2Map m) =
  let k = (k1, k2)
  in atomically $ M.focus (\r -> return (r, F.Remove)) k m
