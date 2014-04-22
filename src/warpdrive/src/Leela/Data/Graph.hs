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

import Leela.Helpers
import Control.Monad
import Leela.Data.Time
import Leela.Data.Types
import Control.Concurrent
import Leela.Storage.Graph
import Control.Concurrent.STM
import Control.Concurrent.Async

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

batch :: [Maybe (IO ())] -> IO ()
batch = go []
    where
      go acc []           = mapM_ wait acc
      go acc (Nothing:xs) = go acc xs
      go acc (Just io:xs)  = do
        a <- async io
        go (a : acc) xs

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

buildQueue :: Time -> Time -> IO (TVar [(Time, Limit)])
buildQueue t0 t1 =
  let (d0, s0) = dateTime t0
      (d1, s1) = dateTime t1
      whithin  = map ((, 24) . flip fromDateTime 0) [(succ d0)..(pred d1)]
  in newTVarIO $ if (d0 == d1)
                   then (t0, max 1 (ceiling $ (s1 - s0) / 3600))
                        : whithin
                   else (t0, 24)
                        : (fromDateTime d1 0, max 1 (ceiling $ s1 / 3600))
                        : whithin

deque :: TVar [a] -> IO (Maybe a)
deque shmem = atomically $ do
  q <- readTVar shmem
  case q of
    []     -> return Nothing
    (x:xs) -> writeTVar shmem xs >> return (Just x)

loadTAttr :: (AttrBackend db) => db -> (Either Int [(Time, Value)] -> IO ()) -> GUID -> Attr -> Time -> Time -> IO ()
loadTAttr db flush guid name t0 t1 = do
  q <- buildQueue t0 t1
  t <- replicateM 4 $ do
    signal <- newEmptyMVar
    void $ forkFinally (doSomeWork q) $ (\me -> do
      putMVar signal ()
      case me of
        Left _ -> flush (Left 599)
        _      -> return ())
    return signal
  mapM_ takeMVar t
    where
      safeFlush (Right []) = return ()
      safeFlush (Right xs) = let ys = filter (\(t,_) -> t>=t0 && t<=t1) xs
                             in when (not $ null ys) (flush $ Right ys)
      safeFlush info       = flush info

      doSomeWork q = do
        work <- deque q
        case work of
          Nothing     -> return ()
          Just (t, l) -> do
            safeFlush =<< liftM Right (getTAttr db guid name t l)
            doSomeWork q

exec :: (GraphBackend db, AttrBackend db) => db -> [Journal] -> IO [(User, Tree, Kind, Node, GUID)]
exec db rt = do
  guids <- async (mapM (mapConcurrently register) (intoChunks 8 $ getPutNode rt))
  batch [ mkio (intoChunks 64 $ getPutLink rt) (mapM_ $ putLink db)
        , mkio (intoChunks 64 $ getPutLabel rt) (mapM_ $ putLabel db)
        , mkio (intoChunks 64 $ getDelLink rt) (mapM_ $ unlink db)
        , mkio (intoChunks 64 $ getPutKAttr rt) (mapM_ $ putAttr db)
        , mkio (intoChunks 64 $ getDelKAttr rt) (mapM_ $ delAttr db)
        , mkio (intoChunks 64 $ getPutTAttr rt) (mapM_ $ putTAttr db)
        ]
  fmap concat (wait guids)

  where
    register (u, t, k, n) = do
      g <- putName db u t k n
      return (u, t, k, n, g)