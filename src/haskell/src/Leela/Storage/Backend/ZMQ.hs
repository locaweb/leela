{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns  #-}

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

module Leela.Storage.Backend.ZMQ
    ( ZMQBackend ()
    , zmqbackend
    ) where

import Data.ByteString (ByteString)
import Control.Exception
import Leela.HZMQ.Dealer
import Leela.Data.Excepts
import Leela.Storage.Graph
import Leela.Storage.Backend.ZMQ.Protocol

data ZMQBackend = ZMQBackend { dealer :: Dealer }

zmqbackend :: Dealer -> ZMQBackend
zmqbackend = ZMQBackend

recv :: Maybe [ByteString] -> Reply
recv Nothing    = FailMsg 500
recv (Just msg) = decode msg

send :: Dealer -> Query -> IO Reply
send pool req = fmap recv (request pool (encode req))

send_ :: Dealer -> Query -> IO ()
send_ pool req = do
  reply <- send pool req
  case reply of
    DoneMsg -> return ()
    _       -> throwIO SystemExcept

instance GraphBackend ZMQBackend where

  getName m g = do
    reply <- send (dealer m) (MsgGetName g)
    case reply of
      NameMsg u t n _ -> return (u, t, n)
      FailMsg 404     -> throwIO NotFoundExcept
      _               -> throwIO SystemExcept

  getGUID m u t n = do
   reply <- send (dealer m) (MsgGetGUID u t n)
   case reply of
     NameMsg _ _ _ g -> return (Just g)
     FailMsg 404     -> return Nothing
     _               -> throwIO SystemExcept

  putName m u t n = do
    reply <- send (dealer m) (MsgPutName u t n)
    case reply of
      NameMsg _ _ _ g -> return g
      _               -> throwIO SystemExcept

  getLabel m g page limit = do
    reply <- send (dealer m) (MsgGetLabel g page limit)
    case reply of
      LabelMsg xs -> return xs
      _           -> throwIO SystemExcept

  putLabel _ []     = return ()
  putLabel m labels = send_ (dealer m) (MsgPutLabel labels)

  hasLink m a l b = do
    reply <- send (dealer m) (MsgHasLink a l b)
    case reply of
      LinkMsg [] -> return False
      LinkMsg _  -> return True
      _          -> throwIO SystemExcept

  getLink m a l page limit = do
    reply <- send (dealer m) (MsgGetLink a l page limit)
    case reply of
      LinkMsg xs -> return xs
      _          -> throwIO SystemExcept

  putLink _ []    = return ()
  putLink m links = send_ (dealer m) (MsgPutLink links)

  unlink _ []    = return ()
  unlink m links = send_ (dealer m) (MsgUnlink links)

  putAttr _ []    = return ()
  putAttr m attrs = send_ (dealer m) (MsgPutAttr attrs)

  getAttr m a k   = do
    reply <- send (dealer m) (MsgGetAttr a k)
    case reply of
      KAttrMsg v  -> return (Just v)
      FailMsg 404 -> return Nothing
      _           -> throwIO SystemExcept

  listAttr m g page limit = do
    reply <- send (dealer m) (MsgListAttr g page limit)
    case reply of
      NAttrMsg xs -> return xs
      _           -> throwIO SystemExcept

  delAttr _ []    = return ()
  delAttr m attrs = send_ (dealer m) (MsgDelAttr attrs)

  remove m a = send_ (dealer m) (MsgDelete a)
