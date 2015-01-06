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

module Leela.DataHelpers where

import Data.Monoid
import Data.Foldable (toList)
import Data.Sequence (fromList)
import Data.ByteString.Lazy (ByteString, empty)
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra

mapToLazyBS :: Int -> [Builder] -> [ByteString]
mapToLazyBS lim = map (toLazyBS lim)

toLazyBS :: Int -> Builder -> ByteString
toLazyBS lim = toLazyByteStringWith (untrimmedStrategy lim smallChunkSize) empty

sConcatMap :: (a -> [b]) -> [a] -> [b]
sConcatMap f = toList . mconcat . map (fromList . f)

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = let (chunk, ys) = splitAt n xs
               in chunk : chunked n ys

chunkSplit :: Int -> [a] -> [[a]]
chunkSplit n xs = filter (not . null) $ foldr zipList (replicate n []) (chunked n xs)
    where
      zipList ys = zipWith ($) (map (:) ys ++ repeat id)
