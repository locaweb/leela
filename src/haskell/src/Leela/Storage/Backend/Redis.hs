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
    ) where

import qualified Data.Aeson as A
import           Database.Redis
import qualified Data.ByteString as B
import           Control.Exception
import           Leela.Data.Excepts
import           Leela.Data.Namespace
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Leela.Storage.Backend

newtype RedisBackend = RedisBackend { ring :: [Connection] }

new :: [ConnectInfo] -> IO RedisBackend
new = fmap RedisBackend . mapM connect

valueof1 :: Either a b -> b
valueof1 = valueof id

valueof_ :: Either a b -> ()
valueof_ = valueof (const ())

valueof :: (b -> c) -> Either a b -> c
valueof f = either (\_ -> throw SystemExcept) f

putnode0 :: Namespace -> Key -> GUID -> Redis ()
putnode0 n k g = fmap valueof_ (hset (unpack g) "" (toStrict $ A.encode (n, k)))

putnode1 :: GUID -> GUID -> Label -> Redis ()
putnode1 a b l = fmap valueof_ (hset (unpack a) (unpack b) (unpack l))

getnode :: GUID -> Redis [(B.ByteString, B.ByteString)]
getnode k = fmap valueof1 (hgetall (unpack k))

getname :: GUID -> Redis (Maybe B.ByteString)
getname k = fmap valueof1 (hget (unpack k) "")

choose :: GUID -> RedisBackend -> IO Connection
choose g cluster = let available = ring cluster
                       size      = fromIntegral (length available)
                       luckyone  = fromIntegral (hash g `mod` size)
                   in return (available !! luckyone)

with :: GUID -> RedisBackend -> Redis a -> IO a
with g cluster exec = do conn <- choose g cluster
                         runRedis conn exec

instance Backend RedisBackend where

    resolve k m = with k m (getname k) >>= asResult
        where asResult Nothing  = throwIO NotFoundExcept
              asResult (Just s) = case (A.decode $ fromStrict s) of
                                    Just v  -> return v
                                    Nothing -> throwIO SystemExcept

    getNode k m = with k m (getnode k) >>= asResult
        where asResult [] = throwIO NotFoundExcept
              asResult xs = return (map (\(a, b) -> (pack a, pack b)) $ filter ((/= "") . fst) xs)

    putNode n k g m = with g m (putnode0 n k g)

    putLink lnks m = mapM_ link lnks
        where link (a, b, l) = with a m (putnode1 a b l)
