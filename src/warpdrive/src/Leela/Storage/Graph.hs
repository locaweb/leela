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
    , AttrEvent (..)
    , GraphEvent (..)
    , LogBackend ()
    , AttrBackend (..)
    , GraphBackend (..)
    , enumKAttrs
    , enumTAttrs
    , defaultLimit
    , logBackend
    , logAttrBackend
    , logGraphBackend
    ) where

import Control.Monad
import Leela.Data.Time
import Leela.Data.Types

data GraphEvent = MakeVertexEvent User Tree Kind Node GUID
                | KillVertexEvent GUID
                | MakeLinkEvent GUID Label GUID
                | KillLinkEvent GUID Label (Maybe GUID)
                deriving (Eq)

data AttrEvent = TAttrPutEvent GUID Attr Time Value [Option]
               | KAttrPutEvent GUID Attr Value [Option]
               | TAttrDelEvent GUID Attr (Maybe Time)
               | KAttrDelEvent GUID Attr
               deriving (Eq)

data LogBackend a = LogBackend { proxy    :: a
                               , ghandler :: GraphEvent -> IO ()
                               , ahandler :: AttrEvent -> IO ()
                               }

type Page = Maybe

type Limit = Int

logGraphBackend :: (GraphBackend a) => (GraphEvent -> IO ()) -> a -> LogBackend a
logGraphBackend handler db = LogBackend db handler (const $ return ())

logAttrBackend :: (AttrBackend a) => (AttrEvent -> IO ()) -> a -> LogBackend a
logAttrBackend handler db = LogBackend db (const $ return ()) handler

logBackend :: (GraphBackend a, AttrBackend a) => (GraphEvent -> IO ()) -> (AttrEvent -> IO ()) -> a -> LogBackend a
logBackend ghandler ahandler db = LogBackend db ghandler ahandler

defaultLimit :: Int
defaultLimit = 512

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

enumAttrs :: (AttrBackend db) => (db -> GUID -> Mode Attr -> Limit -> IO [Attr]) -> db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumAttrs listF db write g mode = do
  values <- listF db g mode defaultLimit
  if (length values < defaultLimit)
    then write values
    else
      unless (null values) $ do
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

instance (AttrBackend m) => AttrBackend (LogBackend m) where

  scanLast = scanLast . proxy

  getTAttr = getTAttr . proxy

  getAttr = getAttr . proxy

  listAttr = listAttr . proxy

  listTAttr = listTAttr . proxy

  putAttr m values = do
    putAttr (proxy m) values
    mapM_ (\(g, a, v, o) -> ahandler m $ KAttrPutEvent g a v o) values

  putTAttr m values = do
    putTAttr (proxy m) values
    mapM_ (\(g, a, t, v, o) -> ahandler m $ TAttrPutEvent g a t v o) values

  delAttr m values = do
    delAttr (proxy m) values
    mapM_ (\(g, a) -> ahandler m $ TAttrDelEvent g a Nothing) values
    mapM_ (\(g, a) -> ahandler m $ KAttrDelEvent g a) values

instance (GraphBackend m) => GraphBackend (LogBackend m) where

  getName = getName . proxy

  getGUID = getGUID . proxy

  hasLink = hasLink . proxy

  getLink = getLink . proxy

  getLabel = getLabel . proxy

  putLabel = putLabel . proxy

  putName m user tree kind node = do
    guid <- putName (proxy m) user tree kind node
    ghandler m $ MakeVertexEvent user tree kind node guid
    return guid

  putLink m links = do
    putLink (proxy m) links
    mapM_ (ghandler m . uncurry3 MakeLinkEvent) links

  unlink m links = do
    unlink (proxy m) links
    mapM_ (ghandler m . uncurry3 KillLinkEvent) links

  remove m guid = do
    remove (proxy m) guid
    ghandler m $ KillVertexEvent guid
