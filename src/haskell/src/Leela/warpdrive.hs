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
import Leela.Config
import Leela.Logger
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

defineEQFlag "warpdrive_endpoint" [|defaultEndpoint :: Endpoint|] "ENDPOINT" "The endpoint to bind to"
defineFlag "warpdrive_capabilities" (64 :: Int) "Number of worker threads"

defineFlag "blackbox_backlog" (32 :: Int) "Maximum number of pending connections"
defineFlag "blackbox_capabilities" (4 :: Int) "Numer of worker threads (per endpoint)"
defineFlag "blackbox_timeout_in_ms" (5000 :: Int) "Maximum allowed time to wait for a response"

signal :: MVar () -> IO ()
signal x = tryPutMVar x () >> return ()

putenv :: Cfg -> IO ()
putenv cfg = do
  cfgWrite cfg Local "warpdrive" [ ("endpoint", fromShow flags_warpdrive_endpoint)
                                 , ("capabilities", fromShow  flags_warpdrive_capabilities)
                                 ]
  cfgWrite cfg Local "blackbox" [ ("backlog", fromShow flags_blackbox_backlog)
                                , ("capabilities", fromShow flags_blackbox_capabilities)
                                , ("timeout_in_ms", fromShow flags_blackbox_timeout_in_ms)
                                ]

main :: IO ()
main = do
  void $ $initHFlags "warpdrive - leela property graph engine"
  cfg   <- cfgOpen flags_zookeeper ["/live/blackbox"]
  alive <- newEmptyMVar
  logsetup flags_debuglevel
  putenv cfg
  void $ installHandler sigTERM (Catch $ signal alive) Nothing
  void $ installHandler sigINT (Catch $ signal alive) Nothing
  lwarn Global "warpdrive: starting..."
  withContext $ \ctx -> do
    withControl $ \ctrl -> do
      storage <- fmap zmqbackend $ create cfg ctx ctrl "blackbox" "/live/blackbox"
      void $ forkFinally (startServer cfg ctx ctrl storage "warpdrive") (\_ -> signal alive)
      takeMVar alive
      cfgClose cfg
  lwarn Global "warpdrive: bye!"
