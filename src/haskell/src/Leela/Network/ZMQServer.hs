{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Network.ZMQServer
       ( startServer
       ) where

import System.ZMQ3
import Leela.HZMQ.Router
import Leela.Network.Core
import Leela.Data.QDevice
import Leela.Data.Endpoint
import Leela.Storage.Backend
import Leela.Network.Protocol

worker :: (GraphBackend m) => m -> CoreServer -> Worker
worker m srv = Worker f (return . encode . encodeE)
  where
    f msg = case (decode msg) of
              Left err -> return $ encode err
              Right q  -> fmap encode (process m srv q)

startServer :: (GraphBackend m) => CoreServer -> Endpoint -> Context -> Control -> m -> IO ()
startServer core addr ctx ctrl storage = startRouter addr ctx ctrl (worker storage core) 
