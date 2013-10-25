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

module Leela.Data.LQL
    ( Using (..)
    , LQL (..)
    , HasNamespace (..)
    ) where

import Leela.Data.Graph
import Leela.Data.Namespace

data Using = Using { uUser  :: Namespace
                   , uRoot  :: (Namespace, Key)
                   }
    deriving (Eq)

data LQL = MakeStmt Using (Result ())
         | PathStmt Using Cursor
         | NameStmt Using GUID

class HasNamespace a where

  root :: a -> Namespace
  self :: a -> Namespace
  top  :: a -> Key

instance HasNamespace Using where

  self (Using _ (n, _)) = n

  root (Using n _) = n

  top (Using _ (_, k)) = k

instance HasNamespace LQL where

  self (MakeStmt r _) = self r
  self (PathStmt r _)  = self r
  self (NameStmt r _)  = self r

  root (MakeStmt r _) = root r
  root (PathStmt r _)  = root r
  root (NameStmt r _)  = root r

  top (MakeStmt r _) = top r
  top (PathStmt r _)  = top r
  top (NameStmt r _)  = top r
