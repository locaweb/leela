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
    ( exec
    , query
    , loadTAttr
    ) where

import qualified Data.IntMap as M
import           Leela.Helpers
import           Control.Monad
import           Leela.Data.Time
import           Leela.Data.Types
import           Control.Concurrent
import           Leela.Storage.Graph
import           Control.Concurrent.Async

query :: (GraphBackend db) => db -> ([(GUID, Label, GUID)] -> IO ()) -> Matcher -> IO ()
query db write (ByEdge a l b) = do
  ok <- hasLink db a l b
  when ok (write [(a, l, b)])
query db write (ByNode a)     = query db write (ByLabel a (Label "*"))
query db write (ByLabel a (Label l0)) = loadLabels (fmap Label $ glob l0)
    where
      loadLabels page = do
        labels <- getLabel db a page defaultLimit
        if (length labels < defaultLimit)
          then mapM_ (flip loadLinks Nothing) labels
          else do
            mapM_ (flip loadLinks Nothing) (init labels)
            loadLabels (nextPage page (last labels))

      loadLinks l page = do
        guids <- getLink db a l page defaultLimit
        if (length guids < defaultLimit)
          then write (map (\b -> (a, l, b)) guids)
          else do
            write (map (\b -> (a, l, b)) (init guids))
            loadLinks l (Just $ last guids)

execute :: [Maybe (IO ())] -> IO ()
execute []             = return ()
execute (Nothing : xs) = execute xs
execute (Just a : xs)  = a >> execute xs

mkio :: [a] -> ([a] -> IO b) -> Maybe (IO b)
mkio [] _ = Nothing
mkio v f  = Just (f v)

getPutNode :: [Journal] -> [(User, Tree, Kind, Node)]
getPutNode = map (\(PutNode u t k n) -> (u, t, k, n)) . filter isPutNode

getPutLabel :: [Journal] -> [(GUID, Label)]
getPutLabel = map (\(PutLabel a l) -> (a, l)) . filter isPutLabel

getPutLink :: [Journal] -> [(GUID, Label, GUID)]
getPutLink = map (\(PutLink a l b) -> (a, l, b)) . filter isPutLink

getDelLink :: [Journal] -> [(GUID, Label, Maybe GUID)]
getDelLink = map (\(DelLink a l mb) -> (a, l, mb)) . filter isDelLink

getPutKAttr :: [Journal] -> [(GUID, Attr, Value, [Option])]
getPutKAttr = map (\(PutKAttr a t v o) -> (a, t, v, o)) . filter isPutKAttr

getDelKAttr :: [Journal] -> [(GUID, Attr)]
getDelKAttr = map (\(DelKAttr a t) -> (a, t)) . filter isDelKAttr

getPutTAttr :: [Journal] -> [(GUID, Attr, Time, Value, [Option])]
getPutTAttr = map (\(PutTAttr g a t v o) -> (g, a, t, v, o)) . filter isPutTAttr

buildQueue :: Time -> Time -> [(Time, Limit)]
buildQueue t0 t1 =
  let dlimit   = 86401
      (d0, _)  = dateTime t0
      (d1, s1) = dateTime t1
      whithin  = map ((, dlimit) . flip fromDateTime 0) [(succ d0)..(pred d1)]
  in if (d0 == d1)
       then [(t0, dlimit)]
       else (t0, dlimit)
            : (whithin ++ [(fromDateTime d1 0, max 1 (ceiling s1))])

loadTAttr :: (AttrBackend db) => db -> ([(Time, Value)] -> IO ()) -> GUID -> Attr -> Time -> Time -> IO ()
loadTAttr db flush guid name t0 t1 = do
  caps   <- fmap (max 2) getNumCapabilities
  memory <- newMVar (0, M.empty)
  void $ mapConcurrently (mapM_ (procData memory)) (chunkSplit caps $ zip [0..] (buildQueue t0 t1))
    where
      flushQueue ix state = do
        case (M.lookup ix state) of
          Nothing -> return (ix, state)
          Just xs -> do
            unless (null xs) (flush xs)
            flushQueue (ix + 1) (M.delete ix state)

      enqueueFlush memory ix xs =
        modifyMVar_ memory $ \(at, state) -> flushQueue at (M.insert ix xs state)

      procData memory (ix, (t, l)) =
        enqueueFlush memory ix =<< getTAttr db guid name t l

exec :: (GraphBackend db, AttrBackend db) => db -> [Journal] -> IO [(User, Tree, Kind, Node, GUID)]
exec db rt = do
  execute [ mkio (chunked 32 $ getPutLink rt) (mapM_ $ putLink db)
          , mkio (chunked 32 $ getPutLabel rt) (mapM_ $ putLabel db)
          , mkio (chunked 32 $ getDelLink rt) (mapM_ $ unlink db)
          , mkio (chunked 32 $ getPutKAttr rt) (mapM_ $ putAttr db)
          , mkio (chunked 32 $ getDelKAttr rt) (mapM_ $ delAttr db)
          , mkio (chunked 32 $ getPutTAttr rt) (mapM_ $ putTAttr db)
          ]
  mapM register (getPutNode rt)
    where
      register (u, t, k, n) = do
        g <- putName db u t k n
        return (u, t, k, n, g)

