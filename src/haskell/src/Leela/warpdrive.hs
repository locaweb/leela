{-# LANGUAGE TemplateHaskell   #-}

-- This file is part of Leela.
--
-- Leela is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Leela is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Leela.  If not, see <http://www.gnu.org/licenses/>.

module Main (main) where

import HFlags
import System.ZMQ3
import Leela.Logger
import Control.Concurrent
import Leela.Storage.Backend
import Leela.Network.ZMQServer as Z
import Leela.Storage.Backend.ZMQ as Zs
import Leela.Storage.Backend.Redis as Rs

import Leela.Data.LQL.Lang ()

defineFlag "redis" "nothing" "The redis server to connect to"

defineFlag "storage" "tcp://127.0.0.1:50021" "The storage to connect to"

defineFlag "endpoint" "tcp://*:50023" "The endpoint to bind to"

defineEQFlag "loglevel" [| NOTICE :: Priority |] "PRIORITY" "The log level"

backend :: Context -> IO AnyBackend
backend ctx = do
  zmqCluster   <- Zs.new ctx [flags_storage]
  if (flags_redis == "nothing")
    then do
      lnotice Global "not using redis backend for caching"
      return $ AnyBackend zmqCluster
    else do
      redisCluster <- Rs.new [flags_redis]
      return $ AnyBackend (AnyBackend redisCluster, AnyBackend zmqCluster)

main :: IO ()
main = withContext $ \ctx -> do
  _    <- $initHFlags "warpdrive - the property graph engine"
  logsetup flags_loglevel
  db   <- backend ctx
  z    <- create db
  wait <- newEmptyMVar
  _    <- forkFinally (zrun z ctx flags_endpoint) (\_ -> putMVar wait ())
  takeMVar wait
  takeMVar wait
