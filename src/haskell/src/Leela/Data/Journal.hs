-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

module Leela.Data.Journal
    ( Journal (..)
    , rechunk
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List (nub)
import           Leela.Data.Namespace

-- | The log of write operations on the graph.  The idea is to provide
-- serialization through logging, which gives us the nice feature of
-- writing asynchronously and also the nasty hazard of
-- read-after-write.
data Journal = PutLink GUID [GUID]
             | PutLabel GUID [Label]
             | DelLink GUID [GUID]
             | PutNode Namespace Key GUID
             deriving (Eq)

partition :: [Journal] -> ([(GUID, [GUID])], [(GUID, [Label])], [(Namespace, Key, GUID)], [(GUID, [GUID])])
partition = go ([], [], [], [])
    where
      go acc []              = acc
      go (a, b, c, d) (x:xs) =
        case x of
          PutLink g gs  -> go ((g,gs) : a, b, c, d) xs
          PutLabel g ls -> go (a, (g,ls) : b, c, d) xs
          PutNode n k g -> go (a, b, (n,k,g) : c, d) xs
          DelLink g gs  -> go (a, b, c, (g,gs) : d) xs

group :: (Ord v) => [(GUID, [v])] -> [(GUID, [v])]
group = map (fmap S.toList) . M.toList . M.fromListWith S.union . map (fmap S.fromList)

rechunk :: [Journal] -> [Journal]
rechunk j = map (uncurry PutLink) (group links)
            ++ map (uncurry PutLabel) (group labels)
            ++ map (\(n, k, g) -> PutNode n k g) (nub nodes)
            ++ map (uncurry DelLink) (group unlinks)
    where
      (links, labels, nodes, unlinks) = partition j
