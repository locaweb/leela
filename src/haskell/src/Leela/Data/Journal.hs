-- This file is part of Leela.
--
-- Leela is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Leela is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Leela.  If not, see <http://www.gnu.org/licenses/>.

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
             | PutNode Namespace Key GUID
             deriving (Eq)

partition :: [Journal] -> ([(GUID, [GUID])], [(GUID, [Label])], [(Namespace, Key, GUID)])
partition = go ([], [], [])
    where
      go acc []           = acc
      go (a, b, c) (x:xs) =
        case x of
          PutLink g gs  -> go ((g,gs) : a, b, c) xs
          PutLabel g ls -> go (a, (g,ls) : b, c) xs
          PutNode n k g -> go (a, b, (n,k,g) : c) xs

group :: (Ord v) => [(GUID, [v])] -> [(GUID, [v])]
group = map (fmap S.toList) . M.toList . M.fromListWith S.union . map (fmap S.fromList)

rechunk :: [Journal] -> [Journal]
rechunk j = map (uncurry PutLink) (group links)
            ++ map (uncurry PutLabel) (group labels)
            ++ map (\(n, k, g) -> PutNode n k g) (nub nodes)
    where
      (links, labels, nodes) = partition j
