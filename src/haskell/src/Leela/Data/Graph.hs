{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

module Leela.Data.Graph
    ( Journal (..)
    , Matcher (..)
    , query
    , update
    ) where

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

data Runtime = Runtime { execPutLink  :: [(GUID, Label, GUID)]
                       , execPutLabel :: [(GUID, Label)]
                       , execPutNode  :: [(User, Tree, Node)]
                       , execDelLink  :: [(GUID, Label, Maybe GUID)]
                       , execDelNode  :: [GUID]
                       }

limit :: Int
limit = 128

query :: (GraphBackend db) => db -> ([(GUID, Label, GUID)] -> IO ()) -> Matcher -> IO ()
query db write (ByEdge a l b) = do
  ok <- hasLink db a l b
  when ok (write [(a, l, b)])
query db write (ByNode a)     = query db write (ByLabel a (Label "*"))
query db write (ByLabel a l0) = loadLabels (glob l0)
    where
      loadLabels page = do
        labels <- getLabel db a page limit
        if (length labels < limit)
          then mapM_ (flip loadLinks Nothing) labels
          else do
            mapM_ (flip loadLinks Nothing) (init labels)
            loadLabels (nextPage page (last labels))

      loadLinks l page = do
        guids <- getLink db a l page limit
        if (length guids < limit)
          then write (map (\b -> (a, l, b)) guids)
          else do
            write (map (\b -> (a, l, b)) (init guids))
            loadLinks l (Just $ last guids)

makeRuntime :: Runtime -> [Journal] -> Runtime
makeRuntime rt []                    = rt
makeRuntime rt (PutLink a l b : xs)  = makeRuntime (rt { execPutLink = (a, l, b) : execPutLink rt }) xs
makeRuntime rt (PutLabel a l : xs)   = makeRuntime (rt { execPutLabel = (a, l) : execPutLabel rt }) xs
makeRuntime rt (PutNode u t n : xs)  = makeRuntime (rt { execPutNode = (u, t, n) : execPutNode rt }) xs
makeRuntime rt (DelLink a l mb : xs) = makeRuntime (rt { execDelLink = (a, l, mb) : execDelLink rt }) xs
makeRuntime rt (DelNode a : xs)      = makeRuntime (rt { execDelNode = a : execDelNode rt }) xs

update :: (GraphBackend db) => db -> [Journal] -> IO [(User, Tree, Node, GUID)]
update db = execute . makeRuntime (Runtime [] [] [] [] [])
  where
    execute rt = do
      a1 <- async (putLink db (execPutLink rt))
      a2 <- async (putLabel db (execPutLabel rt))
      a3 <- async (mapConcurrently myPutName (execPutNode rt))
      a4 <- async (unlink db (execDelLink rt))
      mapM_ wait [a1, a2, a4]
      wait a3

    myPutName (u, t, n) = do
      g <- putName db u t n
      return (u, t, n, g)
