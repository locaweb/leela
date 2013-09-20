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

module Leela.Storage.Backend.Redis
    ( RedisBackend ()
    , new
    , endpointToConnection
    ) where

import qualified Data.Aeson as A
import           Leela.Logger
import           Database.Redis
import qualified Data.ByteString as B
import           Control.Exception
import           Leela.Data.Excepts
import           Leela.Data.Endpoint
import           Leela.Data.Namespace
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import           Leela.Storage.Backend

newtype RedisBackend = RedisBackend { ring :: [Connection] }

nameref :: GUID -> B.ByteString
nameref g = 0x00 `B.cons` (unpack g)

noderef :: GUID -> B.ByteString
noderef g = 0x01 `B.cons` (unpack g)

endpointToConnection :: String -> ConnectInfo
endpointToConnection s =
  case (strEndpoint s) of
    Nothing -> error "unknown endpoint"
    Just e
      | isTCP e   -> ConnInfo (asStr $ eHost e) (asPort (ePort e)) (ePass e) 128 (fromIntegral (300 :: Int))
      | isUNIX e  -> ConnInfo "localhost" (UnixSocket $ asStr $ ePath e) Nothing 128 (fromIntegral (300 :: Int))
      | otherwise -> error "unsupported endpoint"

    where asPort = PortNumber . fromIntegral . maybe 6379 id
          asStr  = C8.unpack

new :: [String] -> IO RedisBackend
new endpoints = do
  lnotice Global $ printf "connecting to redis cluster: %s" (show endpoints)
  fmap RedisBackend $ mapM (connect . endpointToConnection) endpoints

valueof1 :: Monad m => Either a b -> m b
valueof1 = valueof return

valueof_ :: Monad m => Either a b -> m ()
valueof_ = valueof (\_ -> return ())

valueof :: Monad m => (b -> m c) -> Either a b -> m c
valueof f = either (\_ -> fail "redis failure") f

choose :: GUID -> RedisBackend -> IO Connection
choose g cluster =
  let available = ring cluster
      size      = fromIntegral (length available)
      luckyone  = fromIntegral (hash g `mod` size)
  in return (available !! luckyone)

with :: GUID -> RedisBackend -> Redis a -> IO a
with g cluster exec = do
  conn <- choose g cluster
  runRedis conn exec

instance GraphBackend RedisBackend where

  getName g m = with g m (get (nameref g) >>= valueof1) >>= asResult
      where
        asResult Nothing  = throwIO NotFoundExcept
        asResult (Just s) =
          case (A.decode $ L.fromStrict s) of
            Just v  -> return v
            Nothing -> throwIO SystemExcept

  putName n k g m = with g m $ set (nameref g) (L.toStrict $ A.encode (n, k)) >>= valueof_

  getLabel g m = with g m (zrange (noderef g) 0 10000 >>= valueof1) >>= asResult
      where
        asResult [] = throwIO NotFoundExcept
        asResult xs = return $ map pack xs

  putLabel g lbls m = with g m $ zadd (noderef g) (map (\v -> (0, unpack v)) lbls) >>= valueof_

  getLink g m = with g m (zrange (noderef g) 0 10000 >>= valueof1) >>= asResult
      where 
        asResult [] = throwIO NotFoundExcept
        asResult xs = return $ map pack xs

  putLink g lnks m = with g m $ zadd (noderef g) (map (\v -> (0, unpack v)) lnks) >>= valueof_

  unlink g b m = with g m $ zrem (noderef g) [unpack b] >>= valueof_
