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
    ( Page
    , Limit
    , AnyBackend (..)
    , GraphBackend (..)
    ) where

import Leela.Data.Types

data AnyBackend = forall b. (GraphBackend b) => AnyBackend { anyBackend :: b }

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

  putAttr   :: m -> [(GUID, Attr, Value, [Option])] -> IO ()

  getAttr   :: m -> GUID -> Attr -> IO (Maybe Value)

  listAttr  :: m -> GUID -> Mode Attr -> Limit -> IO [Attr]

  delAttr   :: m -> [(GUID, Attr)] -> IO ()

  unlink    :: m -> [(GUID, Label, Maybe GUID)] -> IO ()

  remove    :: m -> GUID -> IO ()

instance GraphBackend AnyBackend where

  getName (AnyBackend db) g = getName db g

  getGUID (AnyBackend db) u t n = getGUID db u t n

  putName (AnyBackend db) u t n = putName db u t n

  getLabel (AnyBackend db) g mode limit = getLabel db g mode limit

  putLabel (AnyBackend db) labels = putLabel db labels

  hasLink (AnyBackend db) a l b = hasLink db a l b

  getLink (AnyBackend db) a l page limit = getLink db a l page limit

  putLink (AnyBackend db) links = putLink db links

  putAttr (AnyBackend db) attrs = putAttr db attrs

  delAttr (AnyBackend db) attrs = delAttr db attrs

  getAttr (AnyBackend db) a k = getAttr db a k

  listAttr (AnyBackend db) a mode limit = listAttr db a mode limit

  unlink (AnyBackend db) links = unlink db links

  remove (AnyBackend db) a = remove db a
