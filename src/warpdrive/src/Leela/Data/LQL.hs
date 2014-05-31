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

module Leela.Data.LQL
    ( Using (..)
    , LQL (..)
    , targetUser
    , groupLQL
    ) where

import Data.List (sortBy)
import Leela.Data.Types

targetUser :: Using -> User
targetUser u = maybe (uUser u) id (uAsUser u)

data Using = Using { uUser   :: User
                   , uTree   :: Tree
                   , uAsUser :: Maybe User
                   }
           deriving (Eq)

data LQL = StatStmt
         | PathStmt (Matcher, [(GUID -> Matcher)])
         | KAttrGetStmt GUID Attr [Option]
         | TAttrGetStmt GUID Attr TimeRange [Option]
         | KAttrListStmt GUID (Mode Attr)
         | TAttrListStmt GUID (Mode Attr)
         | NameStmt Using [GUID]
         | GUIDStmt Using [(Kind, Node)]
         | AlterStmt [Journal]

lqlCmp :: LQL -> LQL -> Ordering
lqlCmp a b = numof a `compare` numof b
    where
      numof StatStmt               = 0
      numof (PathStmt _)           = 1
      numof (KAttrGetStmt _ _ _)   = 2
      numof (TAttrGetStmt _ _ _ _) = 3
      numof (KAttrListStmt _ _)    = 4
      numof (TAttrListStmt _ _)    = 5
      numof (NameStmt _ _)         = 6
      numof (GUIDStmt _ _)         = 7
      numof (AlterStmt _)          = 8

lqlMerge :: LQL -> LQL -> Either LQL (LQL, LQL)
lqlMerge (NameStmt u xs) (NameStmt _ ys) = Left (NameStmt u (xs ++ ys))
lqlMerge (GUIDStmt u xs) (GUIDStmt _ ys) = Left (GUIDStmt u (xs ++ ys))
lqlMerge (AlterStmt xs) (AlterStmt ys)   = Left (AlterStmt (xs ++ ys))
lqlMerge a b                             = Right (a, b)

groupLQL :: [LQL] -> [LQL]
groupLQL = go . sortBy lqlCmp
    where
      go (a : b : xs) = case (lqlMerge a b) of
                          Left a       -> go (a : xs)
                          Right (a, b) -> a : go (b : xs)
      go xs           = xs
