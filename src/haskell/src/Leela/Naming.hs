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

module Leela.Naming
       ( resolve
       , resolver
       , zkResolve
       ) where

import           Data.Maybe
import           Data.IORef
import           Leela.Logger
import           System.Random
import qualified Data.ByteString as B
import           Database.Zookeeper
import           Control.Concurrent
import           Leela.Data.Endpoint

zkResolve :: Zookeeper -> String -> IO [(String, B.ByteString)]
zkResolve zh path0 = go [] [[path0]]
    where
      go acc []                     = return acc
      go acc ([]:backlog)           = go acc backlog
      go acc ((path:paths):backlog) = do
        mvalue    <- get zh path Nothing
        mchildren <- getChildren zh path Nothing
        case mchildren of
          Right children -> go (consVal path mvalue acc) (map (joinPath path) children : paths : backlog)
          Left _         -> return []

      joinPath a b = a ++ "/" ++ b

      consVal _ (Left _)             = id
      consVal _ (Right (Nothing, _)) = id
      consVal k (Right (Just v, _))  = ((k, v) :)

toEndpoints :: B.ByteString -> [Endpoint]
toEndpoints s = catMaybes [ loadEndpoint e | e <- B.split 0xa s ]

resolve :: Endpoint -> String -> IO [Endpoint]
resolve endpoint path =
  withZookeeper (toZookeeper "localhost:2181" endpoint) 5000 Nothing Nothing $ \zh -> do
    linfo Naming "zkReadTree: zookeeper connection established"
    fmap (concatMap (toEndpoints . snd)) (zkResolve zh path)

resolver :: IORef [Endpoint] -> Endpoint -> String -> IO ()
resolver ioref endpoint path = do
  values <- resolve endpoint path
  sleep4 <- randomRIO (15, 60)
  writeIORef ioref values
  linfo Naming (printf "resolver: %s => %s [next-in (s): %d]" path (show values) sleep4)
  threadDelay $ sleep4 * 1000 * 1000
  resolver ioref endpoint path
