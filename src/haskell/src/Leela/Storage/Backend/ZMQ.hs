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

import Data.Maybe
import Data.ByteString (ByteString)
import Control.Exception
import Leela.HZMQ.Dealer
import Control.Concurrent
import Leela.Data.Excepts
import Leela.Data.QDevice
import Leela.Storage.Backend
import Leela.Storage.Backend.ZMQ.Protocol

data ZMQBackend = ZMQBackend { reqPool :: Pool }

new :: Pool -> ZMQBackend
new = ZMQBackend

recvPool :: Maybe [ByteString] -> Reply
recvPool Nothing    = Fail 500
recvPool (Just msg) = decode msg

sendPool :: Pool -> Query -> IO Reply
sendPool pool req = fmap recvPool (request pool (encode req))

instance GraphBackend ZMQBackend where

  getName g m = do
    reply <- sendPool (reqPool m) (GetName g)
    case reply of
      Name n k -> return (n, k)
      Fail 404 -> throwIO NotFoundExcept
      _        -> throwIO SystemExcept

  putName n k g m = do
    reply <- sendPool (reqPool m) (PutName n k g)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  getLabel dev _ (Precise l) _ = do devwriteIO dev (Right [l])
                                    devwriteIO dev (Right [])
  getLabel dev g mode m        = forkFinally (fetch Nothing) (devwriteIO dev) >> return ()
      where
        fetch page = do
          reply <- sendPool (reqPool m) (GetLabel g $ maybe mode id page)
          case reply of
            Label []
              | isNothing page       -> throwIO NotFoundExcept
              | otherwise            -> return []
            Label [x]                -> devwriteIO dev (Right [x]) >> return []
            Label xs
              | length xs < pageSize -> devwriteIO dev (Right xs) >> return []
              | otherwise            -> do devwriteIO dev (Right $ init xs)
                                           case (nextPage mode (last xs)) of
                                             Nothing -> return []
                                             p       -> fetch p
            Fail 404                -> throwIO NotFoundExcept
            _                       -> throwIO SystemExcept

  putLabel g lbls m = do
    reply <- sendPool (reqPool m) (PutLabel g lbls)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  getLink dev g m = forkFinally (fetch Nothing) (devwriteIO dev) >> return ()
      where
        fetch page = do
          reply <- sendPool (reqPool m) (GetLink g page)
          case reply of
            Link []
              | isNothing page       -> throwIO NotFoundExcept
              | otherwise            -> return []
            Link [x]                 -> devwriteIO dev (Right [x]) >> return []
            Link xs
              | length xs < pageSize -> devwriteIO dev (Right xs) >> return []
              | otherwise            -> do devwriteIO dev (Right (init xs))
                                           fetch (Just $ last xs)
            Fail 404                 -> throwIO NotFoundExcept
            _                        -> throwIO SystemExcept

  putLink g lnks m = do
    reply <- sendPool (reqPool m) (PutLink g lnks)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  unlink a b m = do
    reply <- sendPool (reqPool m) (Unlink a b)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept
