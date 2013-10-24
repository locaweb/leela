{-# LANGUAGE TupleSections #-}

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

  getLabel dev _ (Precise l) _ = do devwriteIO dev (Right [l])
                                    devwriteIO dev (Right [])
  getLabel dev g mode m        = void $ forkFinally (fetch Nothing) (devwriteIO dev)
      where
        fetch page = do
          reply <- send (dealer m) (GetLabel g $ maybe mode id page)
          case reply of
            Label []                 -> return []
            Label xs
              | length xs < pageSize -> devwriteIO dev (Right xs) >> return []
              | otherwise            -> do devwriteIO dev (Right $ init xs)
                                           case (nextPage mode (last xs)) of
                                             Nothing -> return []
                                             p       -> fetch p
            _                        -> throwIO SystemExcept

  putLabel g lbls m = do
    reply <- send (dealer m) (PutLabel g lbls)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  getEdge dev guids m = void $ forkFinally (fetch guids) (devwriteIO dev)
      where
        fetch []     = return []
        fetch (x:xs) = do
          reply <- send (dealer m) (uncurry HasLink x)
          case reply of
            Link [] -> fetch xs
            Link _  -> do devwriteIO dev (Right [x])
                          fetch xs
            _       -> throwIO SystemExcept

  getLink dev keys m = void $ forkFinally (fetch keys Nothing) (devwriteIO dev)
      where
        fetch [] _        = return []
        fetch (g:gs) page = do
          reply <- send (dealer m) (GetLink g page)
          case reply of
            Link []                  -> fetch gs Nothing
            Link xs
              | length xs < pageSize -> do devwriteIO dev (Right (map (g, ) xs))
                                           fetch gs Nothing
              | otherwise            -> do devwriteIO dev (Right (map (g, ) (init xs)))
                                           fetch (g:gs) (Just $ last xs)
            _                        -> throwIO SystemExcept

  putLink g lnks m = do
    reply <- send (dealer m) (PutLink g lnks)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept

  unlink a b m = do
    reply <- send (dealer m) (Unlink a b)
    case reply of
      Done -> return ()
      _    -> throwIO SystemExcept
