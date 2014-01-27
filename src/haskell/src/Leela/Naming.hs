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

resolve :: Zookeeper -> String -> IO [Endpoint]
resolve zh path = fmap (concatMap (toEndpoints . snd)) (zkResolve zh path)

resolver :: Endpoint -> IORef [(String, [Endpoint])] -> String -> IO ()
resolver myself ioref endpoint = do
  withZookeeper endpoint 5000 Nothing Nothing $ \zh -> do
    warpdrive <- resolve zh "/naming/warpdrive"
    cassandra <- resolve zh "/naming/cassandra"
    blackbox  <- resolve zh "/naming/blackbox"
    redis     <- resolve zh "/naming/redis"
    atomicWriteIORef ioref [ ("warpdrive", addMyselfIfNull warpdrive)
                           , ("blackbox", blackbox)
                           , ("cassandra", cassandra)
                           , ("redis", redis)
                           ]
  sleep4 <- randomRIO (15, 60)
  linfo Naming (printf "next-in: %d" sleep4)
  threadDelay $ sleep4 * 1000 * 1000
  resolver myself ioref endpoint
    where
      addMyselfIfNull [] = [myself]
      addMyselfIfNull xs = xs
