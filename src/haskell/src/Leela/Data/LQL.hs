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

import qualified Data.ByteString as B
import           Leela.Data.Graph
import           Data.ByteString.Lazy (ByteString)
import           Leela.Data.Namespace

data Using = Using { uUser  :: Namespace
                   , uRoot  :: (Namespace, Key, ByteString)
                   , uAuth  :: Key
                   }
    deriving (Eq)

data LQL = Create Using (Result ())
         | Resolve Using GUID
         | Match Using Cursor

class HasNamespace a where

    root :: a -> Namespace
    self :: a -> Namespace
    top  :: a -> Key

instance HasNamespace Using where

    self (Using _ (n, _, _) _) = n

    root (Using n _ _) = n

    top (Using _ (_, k, _) _) = k

instance HasNamespace LQL where

    self (Create r _)  = self r
    self (Match r _)   = self r
    self (Resolve r _) = self r

    root (Create r _)  = root r
    root (Match r _)   = root r
    root (Resolve r _) = root r

    top (Create r _)  = top r
    top (Match r _)   = top r
    top (Resolve r _) = top r
