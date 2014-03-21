{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

module Leela.Data.Graph
    ( exec
    , query
    , enumKAttrs
    , enumTAttrs
    ) where

import Leela.Helpers
import Control.Monad
import Leela.Data.Time
import Leela.Data.Types
import Leela.Storage.Graph
import Control.Concurrent.Async

limit :: Int
limit = 512

query :: (GraphBackend db) => db -> ([(GUID, Label, GUID)] -> IO ()) -> Matcher -> IO ()
query db write (ByEdge a l b) = do
  ok <- hasLink db a l b
  when ok (write [(a, l, b)])
query db write (ByNode a)     = query db write (ByLabel a (Label "*"))
query db write (ByLabel a (Label l0)) = loadLabels (fmap Label $ glob l0)
    where
      loadLabels page = do
        labels <- getLabel db a page limit
        if (length labels < limit)
          then mapM_ (flip loadLinks Nothing) labels
          else do
            mapM_ (flip loadLinks Nothing) (init labels)
            loadLabels (nextPage page (last labels))

      loadLinks l page = do
        guids <- getLink db a l page limit
        if (length guids < limit)
          then write (map (\b -> (a, l, b)) guids)
          else do
            write (map (\b -> (a, l, b)) (init guids))
            loadLinks l (Just $ last guids)

enumAttrs :: (AttrBackend db) => (db -> GUID -> Mode Attr -> Limit -> IO [Attr]) -> db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumAttrs listF db write g mode = do
  values <- listF db g mode limit
  if (length values < limit)
    then write values
    else do
      write (init values)
      enumAttrs listF db write g (nextPage mode $ last values)

enumKAttrs :: (AttrBackend db) => db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumKAttrs = enumAttrs listAttr

enumTAttrs :: (AttrBackend db) => db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumTAttrs = enumAttrs listTAttr

batch :: [Maybe (IO ())] -> IO ()
batch = go []
    where
      go acc []           = mapM_ wait acc
      go acc (Nothing:xs) = go acc xs
      go acc (Just io:xs)  = do
        a <- async io
        go (a : acc) xs

mkio :: [a] -> ([a] -> IO b) -> Maybe (IO b)
mkio [] _ = Nothing
mkio v f  = Just (f v)

getPutNode :: [Journal] -> [(User, Tree, Node)]
getPutNode = map (\(PutNode u t n) -> (u, t, n)) . filter isPutNode

getPutLabel :: [Journal] -> [(GUID, Label)]
getPutLabel = map (\(PutLabel a l) -> (a, l)) . filter isPutLabel

getPutLink :: [Journal] -> [(GUID, Label, GUID)]
getPutLink = map (\(PutLink a l b) -> (a, l, b)) . filter isPutLink

getDelLink :: [Journal] -> [(GUID, Label, Maybe GUID)]
getDelLink = map (\(DelLink a l mb) -> (a, l, mb)) . filter isDelLink

getPutKAttr :: [Journal] -> [(GUID, Attr, Value, [Option])]
getPutKAttr = map (\(PutKAttr a t v o) -> (a, t, v, o)) . filter isPutKAttr

getDelKAttr :: [Journal] -> [(GUID, Attr)]
getDelKAttr = map (\(DelKAttr a t) -> (a, t)) . filter isDelKAttr

getPutTAttr :: [Journal] -> [(GUID, Attr, Time, Value, [Option])]
getPutTAttr = map (\(PutTAttr g a t v o) -> (g, a, t, v, o)) . filter isPutTAttr

exec :: (GraphBackend db, AttrBackend db) => db -> [Journal] -> IO [(User, Tree, Node, GUID)]
exec db rt = do
  guids <- async (mapM (mapConcurrently register) (intoChunks 8 $ getPutNode rt))
  batch [ mkio (intoChunks 64 $ getPutLink rt) (mapM_ $ putLink db)
        , mkio (intoChunks 64 $ getPutLabel rt) (mapM_ $ putLabel db)
        , mkio (intoChunks 64 $ getDelLink rt) (mapM_ $ unlink db)
        , mkio (intoChunks 64 $ getPutKAttr rt) (mapM_ $ putAttr db)
        , mkio (intoChunks 64 $ getDelKAttr rt) (mapM_ $ delAttr db)
        , mkio (intoChunks 64 $ getPutTAttr rt) (mapM_ $ putTAttr db)
        ]
  fmap concat (wait guids)

  where
    register (u, t, n) = do
      g <- putName db u t n
      return (u, t, n, g)
