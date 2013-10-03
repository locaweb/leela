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

module Leela.Network.ZMQServer
       ( startServer
       ) where

import System.ZMQ3
import Leela.HZMQ.Router
import Leela.Network.Core
import Leela.Storage.Backend
import Leela.Network.Protocol

worker :: (GraphBackend m) => m -> CoreServer -> Worker
worker m srv = Worker f (return . encode . encodeE)
  where
    f msg = case (decode msg) of
              Left err -> return $ encode err
              Right q  -> fmap encode (process m srv q)

startServer :: (GraphBackend m) => m -> Context -> String -> IO ()
startServer m ctx addr = do
  srv <- new
  startRouter "zmqserver" (defaultCfg {endpoint = addr}) (worker m srv) ctx
