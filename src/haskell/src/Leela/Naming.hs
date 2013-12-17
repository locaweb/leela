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

module Leela.Naming
       ( resolve
       , resolver
       , zkResolve
       ) where

import           Data.Maybe
import           Data.IORef
import           Leela.Logger
import           System.Random
import qualified Data.ByteString as B
import           Database.Zookeeper
import           Control.Concurrent
import           Leela.Data.Endpoint

zkResolve :: Zookeeper -> String -> IO [(String, B.ByteString)]
zkResolve zh path0 = go [] [[path0]]
    where
      go acc []                     = return acc
      go acc ([]:backlog)           = go acc backlog
      go acc ((path:paths):backlog) = do
        mvalue    <- get zh path Nothing
        mchildren <- getChildren zh path Nothing
        case mchildren of
          Right children -> go (consVal path mvalue acc) (map (joinPath path) children : paths : backlog)
          Left _         -> return []

      joinPath a b = a ++ "/" ++ b

      consVal _ (Left _)             = id
      consVal _ (Right (Nothing, _)) = id
      consVal k (Right (Just v, _))  = ((k, v) :)

toEndpoints :: B.ByteString -> [Endpoint]
toEndpoints s = catMaybes [ loadEndpoint e | e <- B.split 0xa s ]

resolve :: Endpoint -> String -> IO [Endpoint]
resolve endpoint path =
  withZookeeper (toZookeeper "localhost:2181" endpoint) 5000 Nothing Nothing $ \zh -> do
    linfo Naming "zkReadTree: zookeeper connection established"
    fmap (concatMap (toEndpoints . snd)) (zkResolve zh path)

resolver :: IORef [Endpoint] -> Endpoint -> String -> IO ()
resolver ioref endpoint path = do
  values <- resolve endpoint path
  sleep4 <- randomRIO (15, 60)
  writeIORef ioref values
  linfo Naming (printf "resolver: %s => %s [next-in (s): %d]" path (show values) sleep4)
  threadDelay $ sleep4 * 1000 * 1000
  resolver ioref endpoint path
