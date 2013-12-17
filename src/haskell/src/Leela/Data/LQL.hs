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
         | KillStmt Using GUID (Maybe GUID)

class HasNamespace a where

  root :: a -> Namespace
  self :: a -> Namespace
  top  :: a -> Key

instance HasNamespace Using where

  self (Using _ (n, _)) = n

  root (Using n _) = n

  top (Using _ (_, k)) = k

instance HasNamespace LQL where

  self (MakeStmt r _)   = self r
  self (PathStmt r _)   = self r
  self (NameStmt r _)   = self r
  self (KillStmt r _ _) = self r

  root (MakeStmt r _)   = root r
  root (PathStmt r _)   = root r
  root (NameStmt r _)   = root r
  root (KillStmt r _ _) = root r

  top (MakeStmt r _)   = top r
  top (PathStmt r _)   = top r
  top (NameStmt r _)   = top r
  top (KillStmt r _ _) = top r
