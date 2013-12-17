{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns  #-}

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
recv Nothing    = Fail 500
recv (Just msg) = decode msg

send :: Dealer -> Query -> IO Reply
send pool req = fmap recv (request pool (encode req))

instance GraphBackend ZMQBackend where

  getName g m = do
    reply <- send (dealer m) (GetName g)
    case reply of
      Name n k -> return (n, k)
      Fail 404 -> throwIO NotFoundExcept
      _        -> throwIO SystemExcept

  putName n k g m = do
    reply <- send (dealer m) (PutName n k g)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  getLabel dev g mode m        = void $ forkFinally (fetch 0 Nothing) (devwriteIO dev)
      where
        fetch !at page = do
          reply <- send (dealer m) (GetLabel g $ maybe mode id page)
          case reply of
            Label []                 -> return (at, [])
            Label xs
              | length xs < pageSize -> devwriteIO dev (Right (at, xs)) >> return (at+1, [])
              | otherwise            -> do devwriteIO dev (Right (at, init xs))
                                           case (nextPage mode (last xs)) of
                                             Nothing -> return (at + 1, [])
                                             p       -> fetch (at + 1) p
            _                        -> throwIO SystemExcept

  putLabel g lbls m = do
    reply <- send (dealer m) (PutLabel g lbls)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  getEdge dev guids m = void $ forkFinally (fetch 0 guids) (devwriteIO dev)
      where
        fetch !at []     = return (at, [])
        fetch !at (x:xs) = do
          reply <- send (dealer m) (uncurry HasLink x)
          case reply of
            Link [] -> fetch at xs
            Link _  -> do devwriteIO dev (Right (at, [x]))
                          fetch (at + 1) xs
            _       -> throwIO SystemExcept

  getLink dev keys m = void $ forkFinally (fetch 0 keys Nothing) (devwriteIO dev)
      where
        fetch !at [] _        = return (at, [])
        fetch !at (g:gs) page = do
          reply <- send (dealer m) (GetLink g page)
          case reply of
            Link []                  -> fetch at gs Nothing
            Link xs
              | length xs < pageSize -> do devwriteIO dev (Right (at, map (g, ) xs))
                                           fetch (at + 1) gs Nothing
              | otherwise            -> do devwriteIO dev (Right (at, map (g, ) (init xs)))
                                           fetch (at + 1) (g:gs) (Just $ last xs)
            _                        -> throwIO SystemExcept

  putLink g lnks m = do
    reply <- send (dealer m) (PutLink g lnks)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  unlink a mb m = do
    reply <- send (dealer m) (Unlink a mb)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept
