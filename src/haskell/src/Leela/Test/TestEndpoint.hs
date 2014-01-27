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

module Leela.Test.TestEndpoint
       ( suite
       ) where

import Test.Tasty
import Test.Tasty.HUnit
import Leela.Data.Endpoint

suite :: TestTree
suite = testGroup "Endpoint"
  [ testGroup "TCP"
    [ testCase "host + port" $
        (loadEndpointStr "tcp://localhost:50021") @?= (Just $ TCP "localhost" 50021 "")
    , testCase "host + port + path" $
        (loadEndpointStr "tcp://localhost:50021/path") @?= (Just $ TCP "localhost" 50021 "/path")
    ]
  , testGroup "UDP"
    [ testCase "host + port" $
        (loadEndpointStr "udp://localhost:50021") @?= (Just $ UDP "localhost" 50021 "")
    , testCase "host + port + path" $
        (loadEndpointStr "udp://localhost:50021/path") @?= (Just $ UDP "localhost" 50021 "/path")
    ]
  ]
