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
    ( query
    , update
    , enumAttrs
    ) where

import Control.Monad
import Leela.Data.Types
import Leela.Storage.Backend
import Control.Concurrent.Async

data Batch = Batch { bPutLink  :: [(GUID, Label, GUID)]
                   , bPutLabel :: [(GUID, Label)]
                   , bPutNode  :: [(User, Tree, Node)]
                   , bUnlinks  :: [(GUID, Label, Maybe GUID)]
                   , bPutAttr  :: [(GUID, Attr, Value, [Option])]
                   , bDelAttr  :: [(GUID, Attr)]
                   , bDelNode  :: [GUID]
                   }

limit :: Int
limit = 512

query :: (GraphBackend db) => db -> ([(GUID, Label, GUID)] -> IO ()) -> Matcher -> IO ()
query db write (ByEdge a l b) = do
  ok <- hasLink db a l b
  when ok (write [(a, l, b)])
query db write (ByNode a)     = query db write (ByLabel a (Label "*"))
query db write (ByLabel a (Label l0)) = loadLabels (fmap Label $ glob l0)
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

enumAttrs :: (GraphBackend db) => db -> ([Attr] -> IO ()) -> GUID -> Mode Attr -> IO ()
enumAttrs db write g mode = do
  values <- listAttr db g mode limit
  if (length values < limit)
    then write values
    else do
      write (init values)
      enumAttrs db write g (nextPage mode $ last values)

intoChunks :: Int -> [a] -> [[a]]
intoChunks _ [] = []
intoChunks n l  = let (a, rest) = splitAt n l
                  in a : intoChunks n rest

plan :: Batch -> [Journal] -> Batch
plan rt []                     = rt
plan rt (PutLink a l b : xs)   = plan (rt { bPutLink = (a, l, b) : bPutLink rt }) xs
plan rt (PutLabel a l : xs)    = plan (rt { bPutLabel = (a, l) : bPutLabel rt }) xs
plan rt (PutNode u t n : xs)   = plan (rt { bPutNode = (u, t, n) : bPutNode rt }) xs
plan rt (DelLink a l mb : xs)  = plan (rt { bUnlinks = (a, l, mb) : bUnlinks rt }) xs
plan rt (DelNode a : xs)       = plan (rt { bDelNode = a : bDelNode rt }) xs
plan rt (PutAttr g a v o : xs) = plan (rt { bPutAttr = (g, a, v, o) : bPutAttr rt}) xs
plan rt (DelAttr g a : xs)     = plan (rt { bDelAttr = (g, a) : bDelAttr rt}) xs

exec :: (GraphBackend db) => db -> Batch -> IO [(User, Tree, Node, GUID)]
exec db rt = do
  a1 <- async (mapM_ (putLink db) (intoChunks 100 $ bPutLink rt))
  a2 <- async (mapM_ (putLabel db) (intoChunks 100 $ bPutLabel rt))
  a3 <- async (mapM (mapConcurrently myPutName) (intoChunks 8 $ bPutNode rt))
  a4 <- async (mapM_ (unlink db) (intoChunks 8 $ bUnlinks rt))
  a5 <- async (mapM_ (putAttr db) (intoChunks 100 $ bPutAttr rt))
  a6 <- async (mapM_ (delAttr db) (intoChunks 8 $ bDelAttr rt))
  mapM_ wait [a1, a2, a4, a5, a6]
  liftM concat (wait a3)

  where
    myPutName (u, t, n) = do
      g <- putName db u t n
      return (u, t, n, g)

update :: (GraphBackend db) => db -> [Journal] -> IO [(User, Tree, Node, GUID)]
update db = exec db . plan (Batch [] [] [] [] [] [] [])
