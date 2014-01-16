{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

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

  getName   :: GUID -> m -> IO (User, Tree, Node)

  getGUID   :: User -> Tree -> Node -> m -> IO (Maybe GUID)

  putName   :: User -> Tree -> Node -> m -> IO GUID

  hasLink   :: GUID -> Label -> GUID -> m -> IO Bool

  getLink   :: GUID -> Label -> Page GUID -> Limit -> m -> IO [GUID]

  putLink   :: GUID -> Label -> GUID -> m -> IO ()

  getLabel  :: GUID -> Mode Label -> Limit -> m -> IO [Label]

  putLabel  :: GUID -> Label -> m -> IO ()

  unlink    :: GUID -> Label -> Maybe GUID -> m -> IO ()

  remove    :: GUID -> m -> IO ()

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

  getName g (AnyBackend db) = getName g db

  getGUID u t n (AnyBackend db) = getGUID u t n db

  putName u t n (AnyBackend db) = putName u t n db

  getLabel g mode limit (AnyBackend db) = getLabel g mode limit db

  putLabel g l (AnyBackend db) = putLabel g l db

  hasLink a l b (AnyBackend db) = hasLink a l b db

  getLink a l page limit (AnyBackend db) = getLink a l page limit db

  putLink a l b (AnyBackend db) = putLink a l b db

  unlink a l mb (AnyBackend db) = unlink a l mb db

  remove a (AnyBackend db) = remove a db
