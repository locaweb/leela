{-# LANGUAGE OverloadedStrings #-}

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

import System.ZMQ3
import Leela.Logger
import Database.Redis as R
import Control.Concurrent
import Leela.Network.ZMQServer as Z
import Leela.Storage.Backend.ZMQ as Zs
import Leela.Storage.Backend.Cache as Cs
import Leela.Storage.Backend.Redis as Rs

import Leela.Data.LQL.Lang ()

backend :: Context -> IO (CacheBackend RedisBackend ZMQBackend)
backend ctx = do redisCluster <- Rs.new [defaultConnectInfo { connectPort = UnixSocket "/tmp/redis.socket" }]
                 zmqCluster   <- Zs.new ctx ["tcp://127.0.0.1:9999"]
                 return (Cs.new redisCluster zmqCluster)

main :: IO ()
main = withContext $ \ctx -> do
    logsetup DEBUG
    db   <- backend ctx
    z    <- create db
    wait <- newEmptyMVar
    _    <- forkFinally (zrun z ctx "tcp://*:4080") (\_ -> putMVar wait ())
    takeMVar wait
    takeMVar wait

