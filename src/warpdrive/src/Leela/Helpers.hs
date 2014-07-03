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
supervise syslog name io = do
  (forever io) `catch` restart
    where
      restart (SomeException e) = do
        warning syslog (printf "supervised thread [%s] has died, restarting: %s" name (show e))
        threadDelay (1 * 1000000)

sConcatMap :: (a -> [b]) -> [a] -> [b]
sConcatMap f = toList . mconcat . map (fromList . f)

ignore :: SomeException -> IO ()
ignore _ = return ()

foreverWith :: IO Bool -> IO () -> IO ()
foreverWith check io = do
  ok <- check
  when ok (io >> foreverWith check io)

showDouble :: Double -> String
showDouble = unpack . toShortest

intoChunks :: Int -> [a] -> [[a]]
intoChunks _ [] = []
intoChunks n l  = let (a, rest) = splitAt n l
                  in a : intoChunks n rest
