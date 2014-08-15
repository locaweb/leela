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

module Leela.Storage.Graph
    ( Page
    , Limit
    , AttrBackend (..)
    , GraphBackend (..)
    , defaultLimit
    , enumKAttrs
    , enumTAttrs
    ) where

import Control.Monad
import Leela.Data.Time
import Leela.Data.Types

type Page = Maybe

type Limit = Int

defaultLimit :: Int
defaultLimit = 512

enumAttrs :: (AttrBackend db) => (db -> GUID -> Mode Attr -> Limit -> IO [Attr]) -> db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumAttrs listF db write g mode = do
  values <- listF db g mode defaultLimit
  if (length values < defaultLimit)
    then write values
    else
      when (not $ null values) $ do
        write (init values)
        enumAttrs listF db write g (nextPage mode $ last values)

enumKAttrs :: (AttrBackend db) => db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumKAttrs = enumAttrs listAttr

enumTAttrs :: (AttrBackend db) => db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumTAttrs = enumAttrs listTAttr

class GraphBackend m where

  getName   :: m -> [GUID] -> IO [(User, Tree, Kind, Node, GUID)]

  getGUID   :: m -> [(User, Tree, Kind, Node)] -> IO [(User, Tree, Kind, Node, GUID)]

  putName   :: m -> User -> Tree -> Kind -> Node -> IO GUID

  hasLink   :: m -> GUID -> Label -> GUID -> IO Bool

  getLink   :: m -> GUID -> Label -> Page GUID -> Limit -> IO [GUID]

  putLink   :: m -> [(GUID, Label, GUID)] -> IO ()

  getLabel  :: m -> GUID -> Mode Label -> Limit -> IO [Label]

  putLabel  :: m -> [(GUID, Label)] -> IO ()

  unlink    :: m -> [(GUID, Label, Maybe GUID)] -> IO ()

  remove    :: m -> GUID -> IO ()

class AttrBackend m where

  putAttr   :: m -> [(GUID, Attr, Value, [Option])] -> IO ()

  putTAttr  :: m -> [(GUID, Attr, Time, Value, [Option])] -> IO ()

  scanLast  :: m -> Maybe GUID -> Attr -> (Maybe [(GUID, Attr, Time, Value)] -> IO ()) -> IO ()

  getTAttr  :: m -> GUID -> Attr -> Time -> Limit -> IO [(Time, Value)]

  getAttr   :: m -> GUID -> Attr -> IO (Maybe Value)

  listAttr  :: m -> GUID -> Mode Attr -> Limit -> IO [Attr]

  listTAttr :: m -> GUID -> Mode Attr -> Limit -> IO [Attr]

  delAttr   :: m -> [(GUID, Attr)] -> IO ()
