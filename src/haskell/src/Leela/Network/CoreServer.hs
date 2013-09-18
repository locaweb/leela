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

writeJournal :: (Backend m) => m -> Journal -> IO ()
writeJournal m (PutNode n k g) = putNode n k g m
writeJournal m (PutLink lnks)  = putLink lnks m

load :: (Backend m) => m -> Matcher r -> IO r
load m (ByNode k f) = getNode k m >>= return . f . G.context k

eval :: (Backend m) => [Journal] -> m -> Result r -> IO ([Journal], r)
eval _ _ (G.Fail _)     = throwIO SystemExcept
eval acc _ (G.Done r j) = return (jmerge (j ++ acc), r)
eval acc m (G.Load f _) = load m f >>= eval acc m

dig :: (Backend m) => m -> GUID -> IO (Namespace, Key)
dig m g = resolve g m

perform :: (Backend m) => m -> StreamWriter Reply -> [LQL] -> IO ()
perform m write = exec []
     where exec acc []                  = do mapM_ (writeJournal m) acc
                                             write done
           exec acc ((Create _ r):lql)  = do (acc1, _) <- eval acc m r
                                             exec acc1 lql
           exec acc ((Match _ r):lql)   = do runq [r]
                                             exec acc lql
           exec acc ((Resolve _ g):lql) = do k <- dig m g
                                             write (fromGUID g k)
                                             exec acc lql

           runq []     = return ()
           runq (c:cs) = case c of
                           G.EOF            -> runq cs
                           G.Need q         -> do (_, cursor) <- eval [] m q
                                                  runq (cursor : cs)
                           G.ItemL _ l cont -> do write (fromLink l)
                                                  runq (cont : cs)
                           G.ItemK p cont   -> do write (fromPath p)
                                                  runq (cont : cs)

