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
    , jmerge
    ) where

import Data.List
import Leela.Data.Namespace

-- | The log of write operations on the graph.  The idea is to provide
-- serialization through logging, which gives us the nice feature of
-- writing asynchronously and also the nasty hazard of
-- read-after-write.
data Journal = PutLink [(GUID, GUID, Label)]
             | PutNode Namespace Key GUID

splitType :: [Journal] -> ([Journal], [Journal])
splitType = partition f
    where f (PutNode _ _ _) = True
          f _               = False

jmerge_ :: [Journal] -> [Journal]
jmerge_ []                                     = []
jmerge_ [j]                                    = [j]
jmerge_ (j@(PutNode _ _ _):js)                 = j : jmerge_ js
jmerge_ ((PutLink l0):(PutLink l1):js)         = jmerge_ (PutLink (l0 ++ l1) : js)
jmerge_ (j0@(PutLink _):j1@(PutNode _ _ _):js) = j1 : jmerge_ (j0 : js)

jmerge :: [Journal] -> [Journal]
jmerge js = let (a, b) = splitType js
            in jmerge_ a ++ jmerge_ b
