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

module Leela.Network.CoreServer
    ( FlowControl (..)
    , StreamWriter ()
    , perform
    ) where

import           Control.Exception
import           Leela.Data.LQL
import           Leela.Data.Graph (Matcher (..) , Result)
import qualified Leela.Data.Graph as G
import           Leela.Data.Journal
import           Leela.Data.Excepts
import           Leela.Data.Namespace
import           Leela.Storage.Backend
import           Leela.Network.Protocol

data FlowControl = Stop
                 | Cont

type StreamWriter a = a -> IO ()

store :: (GraphBackend m) => m -> Journal -> IO ()
store m (PutNode n k g)  = putName n k g m
store m (PutLabel lbls)  = mapM_ (\(a, l) -> putLabel a [l] m) lbls
store m (PutLink lnks)   = mapM_ (\(a, b) -> putLink a [b] m) lnks

load :: (GraphBackend m) => m -> Matcher r -> IO r
load m (ByNode k f)  = fmap f (getLabel k m)
load m (ByLabel k f) = fmap f (getLink k m)

eval :: (GraphBackend m) => [Journal] -> m -> Result r -> IO ([Journal], r)
eval _ _ (G.Fail _)     = throwIO SystemExcept
eval acc _ (G.Done r j) = return (j ++ acc, r)
eval acc m (G.Load f _) = load m f >>= eval acc m

deref :: (GraphBackend m) => m -> GUID -> IO (Namespace, Key)
deref m g = getName g m

perform :: (GraphBackend m) => m -> StreamWriter Reply -> [LQL] -> IO ()
perform m write = exec []
     where
       exec acc []                 = do
         mapM_ (store m) acc
         write done
       exec acc ((Create _ r):lql) = do
         (acc1, _) <- eval acc m r
         exec acc1 lql
       exec acc ((Match _ r):lql)  = do
         runq [r]
         exec acc lql
       exec acc ((Deref _ g):lql)  = do
         k <- deref m g
         write (fromGUID g k)
         exec acc lql

       runq []        = return ()
       runq (c:cs) =
         case c of
           G.EOF
             -> runq cs
           G.Need q
             -> do (_, cursor) <- eval [] m q
                   runq (cursor : cs)
           G.Item path nodes cont
             -> do mapM_ (write . flip fromPath path) nodes
                   runq (cont : cs)
