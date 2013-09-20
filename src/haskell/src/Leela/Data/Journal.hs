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
    ) where

import Leela.Data.Namespace

-- | The log of write operations on the graph.  The idea is to provide
-- serialization through logging, which gives us the nice feature of
-- writing asynchronously and also the nasty hazard of
-- read-after-write.
data Journal = PutLink [(GUID, GUID)]
             | PutLabel [(GUID, Label)]
             | PutNode Namespace Key GUID
