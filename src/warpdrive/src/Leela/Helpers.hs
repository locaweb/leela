-- Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Leela.Helpers where

import Data.Monoid
import Leela.Logger
import Data.Foldable (toList)
import Data.Sequence (fromList)
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.ByteString.Lazy (ByteString, empty)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import Data.Double.Conversion.ByteString

mapToLazyBS :: Int -> [Builder] -> [ByteString]
mapToLazyBS lim = map (toLazyBS lim)

toLazyBS :: Int -> Builder -> ByteString
toLazyBS lim = toLazyByteStringWith (untrimmedStrategy lim smallChunkSize) empty

supervise :: Logger -> String -> IO () -> IO ()
supervise syslog name io = io `catch` restart
    where
      restart e =
        case (fromException e) of
          Just ThreadKilled -> return ()
          _                 -> do
            warning syslog (printf "%s: supervised function has died, restarting: %s" name (show e))
            threadDelay (250 * 1000)
            supervise syslog name io

supervise_ :: IO () -> IO ()
supervise_ = supervise devNull ""

sConcatMap :: (a -> [b]) -> [a] -> [b]
sConcatMap f = toList . mconcat . map (fromList . f)

mapMaybeM :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeM f = go []
    where
      go acc []     = return acc
      go acc (a:as) = do
        mb <- f a
        case mb of
          Just b  -> go (b : acc) as
          Nothing -> go acc as

ignore :: SomeException -> IO ()
ignore _ = return ()

foreverWith :: IO Bool -> IO () -> IO ()
foreverWith check io = do
  ok <- check
  when ok (io >> foreverWith check io)

showDouble :: Double -> String
showDouble = unpack . toShortest

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = let (chunk, ys) = splitAt n xs
               in chunk : chunked n ys

chunkSplit :: Int -> [a] -> [[a]]
chunkSplit n xs = filter (not . null) $ foldr zipList (replicate n []) (chunked n xs)
    where
      zipList ys = zipWith ($) (map (:) ys ++ repeat id)
