{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Network.WarpServer
       ( warpServer
       , stopRouter
       , warpLogServer
       ) where

import           System.ZMQ4
import           Leela.Data.Time
import           Control.Exception
import           Leela.HZMQ.Router
import           Leela.HZMQ.Dealer
import           Leela.Network.Core
import           Leela.Data.Endpoint
import           Leela.Storage.Graph
import qualified Data.ByteString.Lazy as L
import           Leela.Storage.Passwd
import           Leela.Network.Protocol
import           Control.Parallel.Strategies

strictEncode :: Reply -> [L.ByteString]
strictEncode = withStrategy (evalList rdeepseq) . encode

worker :: (GraphBackend db, AttrBackend db) => db -> CoreServer -> NominalDiffTime -> Worker
worker db core ttl = Worker f (evaluate . strictEncode . encodeE)
  where
    f msg = do
      time     <- now
      secretdb <- readPasswd core
      case (decode (time, ttl) (readSecret secretdb) msg) of
        Left err -> evaluate $ strictEncode err
        Right q  -> process db core q >>= evaluate . strictEncode

warpServer :: (GraphBackend m, AttrBackend m) => CoreServer -> NominalDiffTime -> Endpoint -> Context -> m -> IO RouterFH
warpServer core ttl addr ctx storage = startRouter (logger core) addr ctx (worker storage core ttl)

warpLogServer :: (GraphBackend m, AttrBackend m) => ClientFH Push -> m -> LogBackend m
warpLogServer client db =
  logBackend handleAttrEvent handleGraphEvent db
    where
      handleAttrEvent _ = return ()

      handleGraphEvent _ = return ()
