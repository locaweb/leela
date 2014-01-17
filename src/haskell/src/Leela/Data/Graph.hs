{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

module Leela.Data.Graph
    ( Journal (..)
    , Matcher (..)
    , query
    , update
    ) where

import Data.Maybe
import Control.Monad
import Leela.Data.Naming
import Leela.Storage.Backend
import Control.Concurrent.Async

data Matcher = ByLabel GUID Label
             | ByNode GUID
             | ByEdge GUID Label GUID
             deriving (Eq)

data Journal = PutLink GUID Label GUID
             | PutLabel GUID Label
             | PutNode User Tree Node
             | DelLink GUID Label (Maybe GUID)
             | DelNode GUID
             deriving (Eq)

limit :: Int
limit = 128

query :: (GraphBackend db) => db -> ([(GUID, Label, GUID)] -> IO ()) -> Matcher -> IO ()
query db write (ByEdge a l b) = do
  ok <- hasLink a l b db
  when ok (write [(a, l, b)])
query db write (ByNode a)     = query db write (ByLabel a (Label "*"))
query db write (ByLabel a l0) = loadLabels (glob l0)
    where
      loadLabels page = do
        labels <- getLabel a page limit db
        if (length labels < limit)
          then mapM_ (flip loadLinks Nothing) labels
          else do
            mapM_ (flip loadLinks Nothing) (init labels)
            loadLabels (nextPage page (last labels))

      loadLinks l page = do
        guids <- getLink a l page limit db
        if (length guids < limit)
          then write (map (\b -> (a, l, b)) guids)
          else do
            write (map (\b -> (a, l, b)) (init guids))
            loadLinks l (Just $ last guids)

group :: Int -> [a] -> [[a]]
group size = go (0, [])
    where
      go (_, []) []     = []
      go (_, xs) []     = reverse xs
      go (k, []) (y:ys) = go (k+1, [[y]]) ys
      go (k, (xs:xss)) (y:ys)
        | k == size     = go (0, [] : reverse xs : xss) (y : ys)
        | otherwise     = go (k + 1, (y : xs) : xss) ys

collect :: [[Maybe a]] -> [a]
collect = concatMap catMaybes

update :: (GraphBackend db) => db -> [Journal] -> IO [(User, Tree, Node, GUID)]
update db = fmap collect . mapM (mapConcurrently exec) . group 8
    where
      exec (PutLink a l b)  = putLink a l b db >> return Nothing
      exec (PutLabel a l)   = putLabel a l db >> return Nothing
      exec (PutNode u t n)  = putName u t n db >>= \g -> return $ Just (u, t, n, g)
      exec (DelLink a l mb) = unlink a l mb db >> return Nothing
      exec (DelNode a)      = remove a db >> return Nothing
