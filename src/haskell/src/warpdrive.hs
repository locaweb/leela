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
defaultZookeeper = TCP [("localhost", Just 2181)] Nothing Nothing "leela"

defaultEndpoint :: Endpoint
defaultEndpoint = TCP [("*", Just 50021)] Nothing Nothing ""

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
