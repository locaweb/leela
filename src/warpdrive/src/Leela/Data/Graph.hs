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
import           Control.Monad
import           Leela.Data.Time
import           Leela.Data.Types
import           Leela.DataHelpers
import           Control.Concurrent
import           Control.Applicative
import           Leela.Storage.Graph
import           Control.Concurrent.Async

query :: (GraphBackend db) => db -> ([(GUID, Label, GUID)] -> IO ()) -> Matcher -> IO ()
query db write (ByNode a)             = query db write (ByLabel (Label "*") a)
query db write (ByLabel (Label l0) a) = loadLabels True (Label <$> glob l0)
    where
      loadLabels first page = do
        labels <- getLabel db a page defaultLimit
        case (length labels) of
          0 -> when first (write [])
          n
            | n < defaultLimit -> mapM_ (uncurry $ loadLinks Nothing) (zip (first : repeat False) labels)
            | otherwise        -> do
                mapM_ (uncurry $ loadLinks Nothing) (zip (first : repeat False) (init labels))
                loadLabels False (nextPage page (last labels))

      loadLinks page first l = do
        guids <- getLink db a l page defaultLimit
        case (length guids) of
          0 -> when first (write [])
          n
            | n < defaultLimit -> write (map (\b -> (a, l, b)) guids)
            | otherwise        -> do
                write (map (\b -> (a, l, b)) (init guids))
                loadLinks (Just $ last guids) False l

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

loadTAttr :: (AttrBackend db) => db -> st -> (st -> [(Time, Value)] -> IO st) -> GUID -> Attr -> Time -> Time -> IO ()
loadTAttr db st0 flush guid name t0 t1 = do
  memory <- newMVar (0, M.empty, st0)
  void $ mapConcurrently (mapM_ (procData memory)) (chunkSplit 4 $ zip [0..] (buildQueue t0 t1))
    where
      flushQueue st ix state =
        case (M.lookup ix state) of
          Nothing -> return (ix, state, st)
          Just [] -> flushQueue st (ix + 1) (M.delete ix state)
          Just xs -> do
            st' <- flush st xs
            flushQueue st' (ix + 1) (M.delete ix state)

      enqueueFlush memory ix xs =
        modifyMVar_ memory $ \(at, state, st) -> flushQueue st at (M.insert ix xs state)

      procData memory (ix, (t, l)) =
        enqueueFlush memory ix =<< loadTAttrs db guid name t l

exec :: (GraphBackend db, AttrBackend db) => db -> [Journal] -> IO [(User, Tree, Kind, Node, GUID)]
exec db rt = do
  caps <- liftM (max 2) getNumCapabilities
  runExec caps 32 (putLink db) (getPutLink rt)
  runExec caps 32 (putLabel db) (getPutLabel rt)
  runExec caps 32 (unlink db) (getDelLink rt)
  runExec caps 32 (delAttr db) (getDelKAttr rt)
  runExec caps 32 (putAttr db) (getPutKAttr rt)
  runExec caps 32 (putTAttr db) (getPutTAttr rt)
  mapM register (getPutNode rt)
    where
      runExec _ _ _ []       = return ()
      runExec caps size f xs = do
        let factor = length xs `div` size
        if factor < 2
          then mapM_ f [xs]
          else void $ mapConcurrently f (chunkSplit (min caps factor) xs)

      register (u, t, k, n) = do
        g <- putName db u t k n
        return (u, t, k, n, g)
