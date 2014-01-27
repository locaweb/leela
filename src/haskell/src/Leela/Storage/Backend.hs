{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

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

module Leela.Storage.Backend
    ( Mode (..)
    , Page
    , Limit
    , AnyBackend (..)
    , GraphBackend (..)
    , glob
    , nextPage
    ) where

import qualified Data.ByteString as B
import           Leela.Data.Naming

data AnyBackend = forall b. (GraphBackend b) => AnyBackend { anyBackend :: b }

data Mode a = All (Maybe a)
            | Prefix a a
            | Suffix a a
            | Precise a

type Page = Maybe

type Limit = Int

class GraphBackend m where

  getName   :: m -> GUID -> IO (User, Tree, Node)

  getGUID   :: m -> User -> Tree -> Node -> IO (Maybe GUID)

  putName   :: m -> User -> Tree -> Node -> IO GUID

  hasLink   :: m -> GUID -> Label -> GUID -> IO Bool

  getLink   :: m -> GUID -> Label -> Page GUID -> Limit -> IO [GUID]

  putLink   :: m -> [(GUID, Label, GUID)] -> IO ()

  getLabel  :: m -> GUID -> Mode Label -> Limit -> IO [Label]

  putLabel  :: m -> [(GUID, Label)] -> IO ()

  unlink    :: m -> [(GUID, Label, Maybe GUID)] -> IO ()

  remove    :: m -> GUID -> IO ()

glob :: Label -> Mode Label
glob l@(Label s)
  | "*" == s           = All Nothing
  | B.isPrefixOf "*" s = uncurry Suffix (range $ B.tail s)
  | B.isSuffixOf "*" s = uncurry Prefix (range $ B.init s)
  | otherwise          = Precise l
    where
      range str = (Label str, Label $ B.init str `B.snoc` (B.last str + 1))

nextPage :: Mode Label -> Label -> Mode Label
nextPage (All _) l      = All (Just l)
nextPage (Prefix _ b) l = Prefix l b
nextPage (Suffix _ b) l = Suffix l b
nextPage _ _            = error "precise has no pagination"

instance GraphBackend AnyBackend where

  getName (AnyBackend db) g = getName db g

  getGUID (AnyBackend db) u t n = getGUID db u t n

  putName (AnyBackend db) u t n = putName db u t n

  getLabel (AnyBackend db) g mode limit = getLabel db g mode limit

  putLabel (AnyBackend db) labels = putLabel db labels

  hasLink (AnyBackend db) a l b = hasLink db a l b

  getLink (AnyBackend db) a l page limit = getLink db a l page limit

  putLink (AnyBackend db) links = putLink db links

  unlink (AnyBackend db) links = unlink db links

  remove (AnyBackend db) a = remove db a
