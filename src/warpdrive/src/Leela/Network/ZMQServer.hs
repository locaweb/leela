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

module Leela.Network.ZMQServer
       ( startServer
       , stopRouter
       ) where

import System.ZMQ4
import Control.Exception
import Leela.HZMQ.Router
import Leela.Network.Core
import Leela.Data.Endpoint
import Leela.Storage.Graph
import Data.ByteString.Lazy (ByteString)
import Leela.Storage.Passwd
import Leela.Storage.KeyValue
import Leela.Network.Protocol
import Control.Parallel.Strategies

strictEncode :: Reply -> [ByteString]
strictEncode = withStrategy (evalList rdeepseq) . encode

worker :: (KeyValue cache, GraphBackend db, AttrBackend db) => cache -> db -> CoreServer -> Worker
worker cache db core = Worker f (evaluate . strictEncode . encodeE)
  where
    f msg = do
      secretdb <- readPasswd core
      case (decode (readSecret secretdb) msg) of
              Left err -> evaluate $ strictEncode err
              Right q  -> process cache db core q >>= evaluate . strictEncode

startServer :: (KeyValue cache, GraphBackend m, AttrBackend m) => CoreServer -> Endpoint -> Context -> cache -> m -> IO RouterFH
startServer core addr ctx cache storage = startRouter (logger core) addr ctx (worker cache storage core)
