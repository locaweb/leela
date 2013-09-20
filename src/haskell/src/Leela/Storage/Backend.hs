{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

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

module Leela.Storage.Backend
    ( GraphBackend (..)
    , AnyBackend (..)
    ) where

import Leela.Logger
import Control.Exception
import Leela.Data.Excepts
import Leela.Data.Namespace
import Control.Concurrent.Async

data AnyBackend = forall b. (GraphBackend b) => AnyBackend { anyBackend :: b }

class GraphBackend m where

  getName   :: GUID -> m -> IO (Namespace, Key)

  putName   :: Namespace -> Key -> GUID -> m -> IO ()

  getLink   :: GUID -> m -> IO [GUID]

  putLink   :: GUID -> [GUID] -> m -> IO ()

  getLabel  :: GUID -> m -> IO [Label]

  putLabel  :: GUID -> [Label] -> m -> IO ()

  unlink    :: GUID -> GUID -> m -> IO ()

nothrow :: IO () -> IO ()
nothrow io = do
  mask $ \restore -> restore io `catch` report
    where report :: SomeException -> IO ()
          report e = lwarn Storage $ printf "error reading/writing storage backend: %s" (show e)

concurrently_ :: IO () -> IO () -> IO ()
concurrently_ ioa iob = concurrently ioa iob >> return ()

tryCache :: (AnyBackend, AnyBackend) -> (AnyBackend -> IO a) -> IO (Either a a)
tryCache (l1, l2) action = do
  result <- try (action l1)
  case result of
    Left e
      | isNotFoundExcept e -> fmap Left $ action l2
      | otherwise          -> do lwarn Storage $ printf "error reading from l1 cache: %s" (show e)
                                 fmap Left $ action l2
    Right a                -> return (Right a)

whenLeft :: Either a a -> (a -> IO ()) -> IO a
whenLeft (Right a) _ = return a
whenLeft (Left a) io = io a >> return a

instance GraphBackend (AnyBackend, AnyBackend) where

  getName g (l1, l2) = do
    result <- tryCache (l1, l2) (getName g)
    whenLeft result (\(n, k) -> nothrow $ putName n k g l1)

  putName n k g (l1, l2) = concurrently_ (nothrow $ putName n k g l1) (putName n k g l2)

  getLabel g (l1, l2) = do
    result <- tryCache (l1, l2) (getLabel g)
    whenLeft result (\l -> nothrow $ putLabel g l l1)

  putLabel g lbls (l1, l2) = concurrently_ (nothrow $ putLabel g lbls l1) (putLabel g lbls l2)

  getLink g (l1, l2) = do
    result <- tryCache (l1, l2) (getLink g)
    whenLeft result (\ns -> nothrow $ putLink g ns l1)

  putLink g lnks (l1, l2) = concurrently_ (nothrow $ putLink g lnks l1) (putLink g lnks l2)

  unlink a b (l1, l2) = concurrently_ (nothrow $ unlink a b l1) (unlink a b l2)

instance GraphBackend AnyBackend where

  getName g (AnyBackend b) = getName g b

  putName n k g (AnyBackend b) = putName n k g b

  getLabel g (AnyBackend b) = getLabel g b

  putLabel g lbls (AnyBackend b) = putLabel g lbls b

  getLink g (AnyBackend b) = getLink g b

  putLink g lnks (AnyBackend b) = putLink g lnks b

  unlink a b (AnyBackend be) = unlink a b be
