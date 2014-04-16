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
    ( timer
    ) where

import           Leela.Helpers
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Types
import           Control.Concurrent
import           Leela.Storage.Graph
import           Control.Concurrent.STM
import           Control.Concurrent.Async

type Key = B.ByteString

compute :: (KeyValue db) => Key -> Time -> Metric -> IO [(Time, Value)]
compute db k t0 (Gauge v)       = [(t0, Double v)]
compute db k t0 (RRDCounter v0) =
  update db k $ \mv ->
    case mv of
      Nothing  -> return (v0, Nothing)
      Just raw ->
        case (decode raw) of
          Left _         -> throwIO SystemExcept
          Right (t1, v1) -> do
            
