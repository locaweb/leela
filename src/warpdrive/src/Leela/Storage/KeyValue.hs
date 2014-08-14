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
    ) where

import           Data.Hashable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type TTL = Int

existsLazy :: (KeyValue m, Hashable k) => m -> k -> L.ByteString  -> IO Bool
existsLazy db sel = exists db sel . L.toStrict

selectLazy :: (KeyValue m, Hashable k) => m -> k -> L.ByteString -> IO (Maybe B.ByteString)
selectLazy db sel = select db sel . L.toStrict

insertLazy :: (KeyValue m, Hashable k) => m -> TTL -> k -> L.ByteString -> B.ByteString -> IO Bool
insertLazy db ttl sel key val = insert db ttl sel (L.toStrict key) val

updateLazy :: (KeyValue m, Hashable k) => m -> TTL -> k -> L.ByteString -> (Maybe B.ByteString -> IO B.ByteString) -> IO B.ByteString
updateLazy db ttl sel key f = update db ttl sel (L.toStrict key) f

class KeyValue m where

  exists    :: (Hashable k) => m -> k -> B.ByteString -> IO Bool
             
  select    :: (Hashable k) => m -> k -> B.ByteString -> IO (Maybe B.ByteString)

  insert    :: (Hashable k) => m -> TTL -> k -> B.ByteString -> B.ByteString -> IO Bool
             
  update    :: (Hashable k) => m -> TTL -> k -> B.ByteString -> (Maybe B.ByteString -> IO B.ByteString) -> IO B.ByteString
