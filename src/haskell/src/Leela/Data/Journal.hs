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

module Leela.Data.Journal
    ( Journal (..)
    ) where

import Leela.Data.Naming

-- | The log of write operations on the graph.  The idea is to provide
-- serialization through logging, which gives us the nice feature of
-- writing asynchronously and also the nasty hazard of
-- read-after-write.
data Journal = PutLink GUID GUID Label
             | PutLabel GUID Label
             | PutNode User Tree Node
             | DelLink GUID (Maybe GUID) Label
             | DelNode GUID
             deriving (Eq)
