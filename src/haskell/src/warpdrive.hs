{-# LANGUAGE TemplateHaskell   #-}

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

module Main (main) where

import HFlags
import Data.IORef
import System.ZMQ3
import Leela.Logger
import Leela.Naming
import Control.Monad
import Leela.HZMQ.Dealer
import Control.Concurrent
import Leela.Data.QDevice
import Leela.Data.Endpoint
import System.Posix.Signals
import Leela.Network.ZMQServer
import Leela.Storage.Backend.ZMQ

defaultZookeeper :: Endpoint
defaultZookeeper = TCP [("localhost", Just 2181)] "leela"

defaultEndpoint :: Endpoint
defaultEndpoint = TCP [("*", Just 50021)] ""

defineEQFlag "debuglevel" [|NOTICE :: Priority|] "LOGLEVEL" "The debug level to use"

defineEQFlag "zookeeper" [|defaultZookeeper :: Endpoint|] "ZOOKEEPER" "The zookeeper cluster to connect to"

defineEQFlag "endpoint" [|defaultEndpoint :: Endpoint|] "ENDPOINT" "The endpoint to bind to"

defineFlag "blackbox_backlog" (32 :: Int) "Maximum number of pending connections"
defineFlag "blackbox_capabilities" (4 :: Int) "Numer of worker threads (per endpoint)"
defineFlag "blackbox_timeout_in_ms" (5000 :: Int) "Maximum allowed time to wait for a response"

signal :: MVar () -> IO ()
signal x = tryPutMVar x () >> return ()

main :: IO ()
main = do
  void $ $initHFlags "warpdrive - leela property graph engine"
  alive    <- newEmptyMVar
  blackbox <- newIORef []
  logsetup flags_debuglevel
  void $ installHandler sigTERM (Catch $ signal alive) Nothing
  void $ installHandler sigINT (Catch $ signal alive) Nothing
  lwarn Global "warpdrive: starting..."
  void $ forkIO $ resolver blackbox flags_zookeeper "/naming/blackbox"
  withContext $ \ctx -> do
    withControl $ \ctrl -> do
      let cfg = DealerConf flags_blackbox_timeout_in_ms
                           flags_blackbox_backlog
                           blackbox
                           flags_blackbox_capabilities
      storage <- fmap zmqbackend $ create cfg ctx ctrl
      void $ forkFinally (startServer flags_endpoint ctx ctrl storage) $ \e -> do
        lwarn Global (printf "warpdrive has died: %s" (show e))
        signal alive
      takeMVar alive
  lwarn Global "warpdrive: bye!"
