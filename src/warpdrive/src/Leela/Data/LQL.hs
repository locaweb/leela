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
    , isStat
    , targetUser
    , lqlDescr
    , groupLQL
    ) where

import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Sequence as S
import           Data.Foldable (toList)
import qualified Data.Map.Strict as M
import           Leela.Data.Time
import           Leela.Data.Types
import           Leela.Data.Pipeline
import           Control.Monad.Identity

targetUser :: Using -> User
targetUser u = fromMaybe (uUser u) (uAsUser u)

data Using = Using { uUser   :: User
                   , uTree   :: Tree
                   , uAsUser :: Maybe User
                   }
           deriving (Eq)

data LQL = StatStmt
         | PathStmt (Matcher, [(GUID -> Matcher)])
         | KAttrGetStmt GUID Attr [Pipeline Identity (V.Vector (Time, Double))]
         | TAttrGetStmt GUID Attr TimeRange [Pipeline Identity (V.Vector (Time, Double))]
         | KAttrListStmt GUID (Mode Attr)
         | TAttrListStmt GUID (Mode Attr)
         | TAttrLastStmt (Maybe GUID) Attr
         | NameStmt Using (S.Seq GUID)
         | GUIDStmt Using (S.Seq (Kind, Node))
         | AlterStmt (S.Seq Journal)

isStat :: LQL -> Bool
isStat StatStmt = True
isStat _        = False

lqlDescr :: [LQL] -> String
lqlDescr = show . go M.empty
    where
      go acc []                      = M.toList acc
      go acc (StatStmt : xs)         = go (M.insertWith (+) "stat" 1 acc) xs
      go acc (PathStmt _ : xs)       = go (M.insertWith (+) "path" 1 acc) xs
      go acc (KAttrGetStmt {} : xs)  = go (M.insertWith (+) "attr get(k)" 1 acc) xs
      go acc (KAttrListStmt {} : xs) = go (M.insertWith (+) "attr kls" 1 acc) xs
      go acc (TAttrListStmt {} : xs) = go (M.insertWith (+) "attr tls" 1 acc) xs
      go acc (TAttrGetStmt {} : xs)  = go (M.insertWith (+) "attr get(t)" 1 acc) xs
      go acc (NameStmt {} : xs)      = go (M.insertWith (+) "name" 1 acc) xs
      go acc (GUIDStmt {} : xs)      = go (M.insertWith (+) "guid" 1 acc) xs
      go acc (TAttrLastStmt {} : xs) = go (M.insertWith (+) "attr last" 1 acc) xs
      go acc (AlterStmt j : xs)      = go (jDescr acc (toList j)) xs

      jDescr acc []                 = acc
      jDescr acc (PutLink {} : xs)  = jDescr (M.insertWith (+) "make(l)" 1 acc) xs
      jDescr acc (PutLabel {} : xs) = jDescr (M.insertWith (+) "make(l)" 1 acc) xs
      jDescr acc (PutNode {} : xs)  = jDescr (M.insertWith (+) "make(n)" 1 acc) xs
      jDescr acc (DelLink {} : xs)  = jDescr (M.insertWith (+) "kill(l)" 1 acc) xs
      jDescr acc (DelNode {} : xs)  = jDescr (M.insertWith (+) "kill(n)" 1 acc) xs
      jDescr acc (DelKAttr {} : xs) = jDescr (M.insertWith (+) "attr kill(k)" 1 acc) xs
      jDescr acc (DelTAttr {} : xs) = jDescr (M.insertWith (+) "attr kill(t)" 1 acc) xs
      jDescr acc (PutKAttr {} : xs) = jDescr (M.insertWith (+) "attr put(k)" 1 acc) xs
      jDescr acc (PutTAttr {} : xs) = jDescr (M.insertWith (+) "attr put(t)" 1 acc) xs

lqlMerge :: LQL -> LQL -> Either LQL (LQL, LQL)
lqlMerge (NameStmt u xs) (NameStmt _ ys) = Left (NameStmt u (xs S.>< ys))
lqlMerge (GUIDStmt u xs) (GUIDStmt _ ys) = Left (GUIDStmt u (xs S.>< ys))
lqlMerge (AlterStmt xs) (AlterStmt ys)   = Left (AlterStmt (xs S.>< ys))
lqlMerge a b                             = Right (a, b)

groupLQL :: [LQL] -> [LQL]
groupLQL = go
    where
      go (a : b : xs) = case (lqlMerge a b) of
                          Left a       -> go (a : xs)
                          Right (a, b) -> a : go (b : xs)
      go xs           = xs
