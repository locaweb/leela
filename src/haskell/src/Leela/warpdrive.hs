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
import Leela.HZMQ.Dealer
import Control.Concurrent
import Leela.Data.QDevice
import System.Posix.Signals
import Leela.Storage.Backend
import Leela.Network.ZMQServer as Z
import Leela.Storage.Backend.ZMQ

defineFlag "storage" "tcp://127.0.0.1:50021" "The storage to connect to"

defineFlag "endpoint" "tcp://*:50023" "The endpoint to bind to"

defineEQFlag "loglevel" [| NOTICE :: Priority |] "PRIORITY" "The log level"

backend :: Control -> Context -> IO AnyBackend
backend ctrl ctx = do
  pool <- create ctrl "zmq.backend" defaultCfg ctx [flags_storage]
  return $ AnyBackend (new pool)

wait :: MVar () -> IO ()
wait = takeMVar

signal :: MVar () -> IO ()
signal x = tryPutMVar x () >> return ()

main :: IO ()
main = do
  lck  <- newEmptyMVar
  _    <- installHandler sigTERM (Catch $ signal lck) Nothing
  _    <- installHandler sigINT (Catch $ signal lck) Nothing
  withContext $ \ctx -> do 
    _    <- $initHFlags "warpdrive - the property graph engine"
    logsetup flags_loglevel
    withControl $ \ctrl -> do
      db   <- backend ctrl ctx
      _    <- forkFinally (startServer ctrl db ctx flags_endpoint) (\_ -> signal lck)
      wait lck
