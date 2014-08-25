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

module Leela.Storage.KeyValue
    ( TTL
    , KeyValue (..)
    , existsLazy
    , selectLazy
    , insertLazy
    , updateLazy
    , scanOnLazy
    , scanAllLazy
    ) where

import           Data.Hashable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type TTL = Int

type Glob = B.ByteString

type GlobLazy = L.ByteString

existsLazy :: (KeyValue m, Hashable k) => m -> k -> L.ByteString  -> IO Bool
existsLazy db sel = exists db sel . L.toStrict

selectLazy :: (KeyValue m, Hashable k) => m -> k -> L.ByteString -> IO (Maybe B.ByteString)
selectLazy db sel = select db sel . L.toStrict

insertLazy :: (KeyValue m, Hashable k) => m -> TTL -> k -> L.ByteString -> B.ByteString -> IO Bool
insertLazy db ttl sel key = insert db ttl sel (L.toStrict key)

updateLazy :: (KeyValue m, Hashable k) => m -> TTL -> k -> L.ByteString -> (Maybe B.ByteString -> IO B.ByteString) -> IO B.ByteString
updateLazy db ttl sel key = update db ttl sel (L.toStrict key)

scanOnLazy :: (KeyValue m, Hashable k) => m -> k -> GlobLazy -> (Maybe [(B.ByteString, B.ByteString)] -> IO ()) -> IO ()
scanOnLazy db sel glob = scanOn db sel (L.toStrict glob)

scanAllLazy :: KeyValue m => m -> GlobLazy -> (Maybe [(B.ByteString, B.ByteString)] -> IO ()) -> IO ()
scanAllLazy db glob = scanAll db (L.toStrict glob)

class KeyValue m where

  exists  :: (Hashable k) => m -> k -> B.ByteString -> IO Bool
             
  select  :: (Hashable k) => m -> k -> B.ByteString -> IO (Maybe B.ByteString)

  insert  :: (Hashable k) => m -> TTL -> k -> B.ByteString -> B.ByteString -> IO Bool
             
  update  :: (Hashable k) => m -> TTL -> k -> B.ByteString -> (Maybe B.ByteString -> IO B.ByteString) -> IO B.ByteString

  scanOn  :: (Hashable k) => m -> k -> Glob -> (Maybe [(B.ByteString, B.ByteString)] -> IO ()) -> IO ()

  scanAll :: m -> Glob -> (Maybe [(B.ByteString, B.ByteString)] -> IO ()) -> IO ()
