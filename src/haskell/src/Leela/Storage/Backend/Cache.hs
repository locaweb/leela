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

module Leela.Storage.Backend.Cache
    ( CacheBackend ()
    , new
    ) where

import Leela.Logger
import Control.Exception
import Leela.Data.Excepts
import Leela.Storage.Backend as B

newtype CacheBackend a b = CacheBackend (a, b)

new :: a -> b -> CacheBackend a b
new a b = CacheBackend (a, b)

onL1 :: CacheBackend a b -> (a -> IO c) -> IO (Either SomeException c)
onL1 (CacheBackend (a, _)) f = try (f a)

onL2 :: CacheBackend a b -> (b -> c) -> c
onL2 (CacheBackend (_, b)) f = f b

tryL1 :: CacheBackend a b
      -> (a -> IO c)
      -> (c -> a -> IO ())
      -> (b -> IO c)
      -> IO c
tryL1 m f h g = do mresult <- onL1 m f
                   case mresult of
                     Left e
                       | isNotFoundExcept e 
                           -> do result <- onL2 m g
                                 _      <- onL1 m (h result)
                                 return result
                       | otherwise
                           -> do linfo Global $ printf "l1 cache has failed: %s" (show e)
                                 onL2 m g
                     Right result
                       -> return result

instance (Backend a, Backend b) => Backend (CacheBackend a b) where

    resolve g m = tryL1 m (resolve g) cache (resolve g)
        where cache (n, k) = putNode n k g

    getNode k m = tryL1 m (getNode k) cache (getNode k)
        where cache links = putLink (map (\(b, l) -> (k, b, l)) links)

    putNode n k g m = do onL2 m (putNode n k g)
                         _ <- onL1 m (putNode n k g)
                         return ()

    putLink lnks m = do onL2 m (putLink lnks)
                        _ <- onL1 m (putLink lnks)
                        return ()
