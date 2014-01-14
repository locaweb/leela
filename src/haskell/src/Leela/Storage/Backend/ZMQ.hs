{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns  #-}

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

module Leela.Storage.Backend.ZMQ
    ( ZMQBackend ()
    , zmqbackend
    ) where

import Control.Monad
import Data.ByteString (ByteString)
import Control.Exception
import Leela.HZMQ.Dealer
import Control.Concurrent
import Leela.Data.Excepts
import Leela.Data.QDevice
import Leela.Storage.Backend
import Leela.Storage.Backend.ZMQ.Protocol

data ZMQBackend = ZMQBackend { dealer :: Dealer }

zmqbackend :: Dealer -> ZMQBackend
zmqbackend = ZMQBackend

recv :: Maybe [ByteString] -> Reply
recv Nothing    = FailMsg 500
recv (Just msg) = decode msg

send :: Dealer -> Query -> IO Reply
send pool req = fmap recv (request pool (encode req))

instance GraphBackend ZMQBackend where

  getName g m = do
    reply <- send (dealer m) (GetName g)
    case reply of
      NameMsg u t n _ -> return (u, t, n)
      FailMsg 404     -> throwIO NotFoundExcept
      _               -> throwIO SystemExcept

  getGUID u t n m = do
   reply <- send (dealer m) (GetGUID u t n)
   case reply of
     NameMsg _ _ _ g -> return (Just g)
     FailMsg 404     -> return Nothing
     _               -> throwIO SystemExcept

  putName u t n m = do
    reply <- send (dealer m) (PutName u t n)
    case reply of
      NameMsg _ _ _ g -> return g
      _               -> throwIO SystemExcept

  getLabel dev g mode m = void $ forkFinally (fetch 0 Nothing) (devwriteIO dev)
      where
        fetch !at page = do
          reply <- send (dealer m) (GetLabel g $ maybe mode id page)
          case reply of
            LabelMsg []              -> return (at, [])
            LabelMsg xs
              | length xs < pageSize -> devwriteIO dev (Right (at, xs)) >> return (at+1, [])
              | otherwise            -> do devwriteIO dev (Right (at, init xs))
                                           case (nextPage mode (last xs)) of
                                             Nothing -> return (at + 1, [])
                                             p       -> fetch (at + 1) p
            _                        -> throwIO SystemExcept

  putLabel g l m = do
    reply <- send (dealer m) (PutLabel g l)
    case reply of
      DoneMsg -> return ()
      _       -> throwIO SystemExcept

  hasLink dev a l b m = void $ forkFinally fetch (devwriteIO dev)
      where
        fetch = do
          reply <- send (dealer m) (HasLink a l b)
          case reply of
            LinkMsg [] -> return (0, [])
            LinkMsg xs -> devwriteIO dev (Right (0, xs)) >> return (0, [])
            _          -> throwIO SystemExcept

  getLink dev a l m = void $ forkFinally (fetch 0 Nothing) (devwriteIO dev)
      where
        fetch !at page = do
          reply <- send (dealer m) (GetLink a l page)
          case reply of
            LinkMsg []               -> return (at, [])
            LinkMsg xs
              | length xs < pageSize -> devwriteIO dev (Right (at, xs)) >> return (at, [])
              | otherwise            -> do devwriteIO dev (Right (at, init xs))
                                           fetch (at + 1) (Just $ last xs)
            _                        -> throwIO SystemExcept

  putLink a l b m = do
    reply <- send (dealer m) (PutLink a l b)
    case reply of
      DoneMsg -> return ()
      _       -> throwIO SystemExcept

  unlink a l mb m = do
    reply <- send (dealer m) (Unlink a l mb)
    case reply of
      DoneMsg -> return ()
      _       -> throwIO SystemExcept

  delete a m = do
    reply <- send (dealer m) (Delete a)
    case reply of
      DoneMsg -> return ()
      _       -> throwIO SystemExcept
