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

module Leela.Storage.Backend.ZMQ
    ( ZMQBackend ()
    , new
    ) where

import Data.Aeson
import System.ZMQ3
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromChunks)
import Control.Exception
import Leela.HZMQ.Dealer
import Leela.Data.Excepts
import Leela.Storage.Backend
import Leela.Storage.Backend.ZMQ.Protocol

data ZMQBackend = ZMQBackend { reqPool :: Pool }

new :: Context -> [String] -> IO ZMQBackend
new ctx endpoints = do
  pool <- create "zmq.backend" defaultCfg ctx endpoints
  return (ZMQBackend pool)

recvPool :: Maybe [ByteString] -> Response
recvPool Nothing    = RFail 500
recvPool (Just msg) = asResponse
    where
      asResponse =
        case (decode $ fromChunks msg) of
          Nothing -> RFail 502
          Just v  -> v

sendPool :: Pool -> Request -> IO Response
sendPool pool req = fmap recvPool (request pool msg)
    where msg = [toStrict $ encode req]

instance GraphBackend ZMQBackend where

  getName g m = do
    reply <- sendPool (reqPool m) (GetName g)
    case reply of
      RName n k -> return (n, k)
      RFail 404 -> throwIO NotFoundExcept
      _         -> throwIO SystemExcept

  putName n k g m = do
    reply <- sendPool (reqPool m) (PutName n k g)
    case reply of
      ROk -> return ()
      _   -> throwIO SystemExcept

  getLabel g m = do
    reply <- sendPool (reqPool m) (GetLabel g)
    case reply of
      RLabel value -> return value
      RFail 404    -> throwIO NotFoundExcept
      _            -> throwIO SystemExcept

  putLabel g lbls m = do
    reply <- sendPool (reqPool m) (PutLabel g lbls)
    case reply of
      ROk -> return ()
      _   -> throwIO SystemExcept

  getLink g m = do
    reply <- sendPool (reqPool m) (GetLink g)
    case reply of
      RLink value -> return value
      RFail 404   -> throwIO NotFoundExcept
      _           -> throwIO SystemExcept

  putLink g lnks m = do
    reply <- sendPool (reqPool m) (PutLink g lnks)
    case reply of
      ROk -> return ()
      _   -> throwIO SystemExcept

  unlink a b m = do
    reply <- sendPool (reqPool m) (Unlink a b)
    case reply of
      ROk -> return ()
      _   -> throwIO SystemExcept
