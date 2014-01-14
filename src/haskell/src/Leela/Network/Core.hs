{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE Rank2Types        #-}

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

module Leela.Network.Core
       ( CoreServer ()
       , newCore
       , process
       ) where

import qualified Data.Map as M
import           Data.IORef
import           Leela.Logger
import           Leela.Helpers
import           Control.Monad
import           Leela.Data.LQL
import qualified Data.ByteString as B
import           Leela.Data.Graph (Matcher (..) , Result)
import qualified Leela.Data.Graph as G
import           Control.Exception
import           Leela.Data.Naming
import           Control.Concurrent
import           Leela.Data.Journal
import           Leela.Data.QDevice
import           Leela.Data.Excepts
import           Leela.Data.Endpoint
import           Leela.Data.LQL.Comp
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.UTF8 (fromString)
import           Leela.Storage.Backend
import           Control.Concurrent.STM
import           Leela.Network.Protocol

data CoreServer = CoreServer { stat   :: IORef [(String, [Endpoint])]
                             , fdseq  :: TVar FH
                             , fdlist :: TVar (M.Map (B.ByteString, FH) (Int, Device Reply))
                             }

data Stream a = Chunk a
              | EOF

ttl :: Int
ttl = 30

dumpStat :: CoreServer -> IO [(B.ByteString, B.ByteString)]
dumpStat core = do
  state <- readIORef (stat core)
  return (concatMap dumpEntry state)
    where
      dumpEntry (k, [])     = [(fromString $ "endpoint/" ++ k, "")]
      dumpEntry (k, [e])    = [(fromString $ "endpoint/" ++ k, toStrict $ dumpEndpoint e)]
      dumpEntry (k, (e:es)) = (fromString $ "endpoint/" ++ k, toStrict $ dumpEndpoint e) : dumpEntry (k, es)

whenChunk :: (Stream a -> IO ()) -> Stream a -> IO ()
whenChunk _ EOF   = return ()
whenChunk f chunk = f chunk

newCore :: IORef [(String, [Endpoint])] -> IO CoreServer
newCore statdb = do
  state <- makeState
  _     <- forkIO (forever (sleep 1 >> rungc (fdlist state)))
  return state
    where
      makeState = atomically $
        liftM2 (CoreServer statdb) (newTVar 0) (newTVar M.empty)

rungc :: (Ord k, Show k) => TVar (M.Map k (Int, Device a)) -> IO ()
rungc tvar = atomically kill >>= mapM_ burry
    where
      partition acc []       = acc
      partition (a, b) ((k, (tick, dev)):xs)
        | tick == 0 = partition ((k, dev) : a, b) xs
        | otherwise = partition (a, (k, (tick - 1, dev)) : b) xs

      kill = do
        (dead, alive) <- fmap (partition ([], []) . M.toList) (readTVar tvar)
        writeTVar tvar (M.fromList alive)
        return dead

      burry (k, dev) = do
        lwarn Network $ printf "closing/purging unused channel: %s" (show k)
        atomically $ close dev

nextfd :: CoreServer -> STM FH
nextfd srv = do
  curr <- readTVar $ fdseq srv
  writeTVar (fdseq srv) (curr + 1)
  return curr

makeFD :: CoreServer -> User -> IO (FH, Device Reply)
makeFD srv (User u) = atomically $ do
  ctrl <- control
  fd   <- nextfd srv
  dev  <- open ctrl pageSize
  modifyTVar (fdlist srv) (M.insert (u, fd) (ttl, dev))
  return (fd, dev)

selectFD :: CoreServer -> (User, FH) -> IO (Maybe (Device Reply))
selectFD srv ((User u), fh) = atomically $ do
  let resetTTL _ (_, dev) = Just (ttl, dev)
  (mdev, newv) <- fmap (M.updateLookupWithKey resetTTL (u, fh)) (readTVar (fdlist srv))
  writeTVar (fdlist srv) newv
  return (fmap snd mdev)

closeFD :: CoreServer -> Bool -> (User, FH) -> IO ()
closeFD srv nowait ((User u), fh) = do
  ldebug Network (printf "closing fd %s" (show k))
  atomically $ do
    db   <- readTVar (fdlist srv)
    case (M.lookup k db) of
      Nothing       -> return ()
      Just (_, dev) -> do unless nowait (linger dev)
                          writeTVar (fdlist srv) (M.delete k db)
                          close dev
    where
      k = (u, fh)

store :: (GraphBackend m) => m -> Journal -> IO ()
store storage (PutNode u t n)   = void $ putName u t n storage
store storage (PutLabel g lbls) = putLabel g lbls storage
store storage (PutLink a b l)   = putLink a l b storage
store storage (DelLink a b l)   = unlink a l b storage
store storage (DelNode a)       = delete a storage

fetch :: (GraphBackend m, HasControl ctrl) => ctrl -> m -> Matcher r -> (Stream r -> IO ()) -> IO ()
fetch ctrl storage selector callback0 =
  case selector of
    ByLabel k l f  -> do dev <- openIO ctrl 2
                         getLabel dev k (glob l) storage
                         request k Nothing f dev
    ByNode k f     -> do dev <- openIO ctrl 2
                         getLabel dev k (All Nothing) storage
                         request k Nothing f dev
    ByEdge a l b f -> do dev <- openIO ctrl 2
                         getLabel dev a (glob l) storage
                         request a (Just b) f dev
    where
      request a mb f dev = do
        mlabels <- devreadIO dev
        case mlabels of
          Nothing              -> throwIO SystemExcept
          Just (Left e)        -> throwIO e
          Just (Right (0, [])) -> callback0 (Chunk $ f [])
          Just (Right (_, [])) -> callback0 EOF
          Just (Right (_, xs)) -> do
            subdev <- openIO ctrl 4
            empty  <- fetchLabels True a mb subdev xs $ \links ->
              callback0 (Chunk $ f links)
            if empty
              then callback0 (Chunk $ f [])
              else callback0 EOF

      fetchLabels empty _ _ _ [] _               = return empty
      fetchLabels empty a mb dev (l:ls) callback = do
        case mb of
          Nothing -> getLink dev a l storage
          Just b  -> hasLink dev a l b storage
        nempty <- fetchLinks empty dev $ \guids ->
          unless (null guids) (callback $ map (, l) guids)
        if (null ls)
          then return nempty
          else fetchLabels nempty a mb dev ls callback

      fetchLinks empty dev callback = do
        mnodes <- devreadIO dev
        case mnodes of
          Nothing                 -> throwIO SystemExcept
          Just (Left e)           -> throwIO e
          Just (Right (_, []))    -> callback [] >> return empty
          Just (Right (_, nodes)) -> callback nodes >> fetchLinks False dev callback

eval :: (GraphBackend m, HasControl ctrl) => ctrl -> m -> Result r -> (Stream r -> IO ()) -> IO ()
eval _ _ (G.Fail 404 _) _         = throwIO NotFoundExcept
eval _ _ (G.Fail code msg) _      = do lwarn Network (printf "eval has failed: %d/%s" code msg)
                                       throwIO SystemExcept
eval ctrl storage (G.Load f g) callback =
  catch (fetch ctrl storage f $ \chunk ->
           case chunk of
             EOF     -> callback EOF
             Chunk r -> eval ctrl storage r (whenChunk callback))
        (\e -> case e of
                 NotFoundExcept -> eval ctrl storage g callback
                 _              -> throwIO e)
eval _ _ (G.Done r) callback      = do
  callback (Chunk r)
  callback EOF

evalLQL :: (GraphBackend m) => m -> CoreServer -> Device Reply -> [LQL] -> IO ()
evalLQL _ _ dev []              = devwriteIO dev (Last Nothing)
evalLQL storage core dev (x:xs) =
  case x of
    PathStmt cursor -> navigate cursor (evalLQL storage core dev xs)
    AlterStmt []    -> evalLQL storage core dev xs
    AlterStmt (PutNode u t n:stmts)
                    -> do
      g <- putName u t n storage
      devwriteIO dev (Item $ Name u t n g)
      evalLQL storage core dev (AlterStmt stmts : xs)
    AlterStmt (stmt:stmts)
                    -> do
      store storage stmt
      evalLQL storage core dev (AlterStmt stmts : xs)
    StatStmt        -> do
      state <- dumpStat core
      devwriteIO dev (Item $ Stat state)
      evalLQL storage core dev xs
    NameStmt _ g    -> do
      (gUser, gTree, gName) <- getName g storage
      devwriteIO dev (Item $ Name gUser gTree gName g)
      evalLQL storage core dev xs
    GUIDStmt u n    -> do
      mg <- getGUID (uUser u) (uTree u) n storage
      case mg of
        Nothing -> devwriteIO dev (Fail 404 Nothing)
        Just g  -> devwriteIO dev (Item $ Name (uUser u) (uTree u) n g) >> evalLQL storage core dev xs
    where
      navigate G.Tail cont     = cont
      navigate (G.Need r) cont = eval dev storage r $ \chunk ->
        case chunk of
          EOF          -> cont
          Chunk cursor -> navigate cursor (return ())
      navigate (G.Item path links next) cont = do
        devwriteIO dev (Item $ makeList $ map (Path . (:path)) links)
        navigate next cont
      navigate (G.Head g) cont =
        eval dev storage (G.loadNode1 g Nothing Nothing G.done) $ \chunk ->
          case chunk of
            EOF         -> cont
            Chunk links -> devwriteIO dev (Item $ makeList $ map (Path . (:[])) links)

evalFinalizer :: FH -> Device Reply -> Either SomeException () -> IO ()
evalFinalizer chan dev (Left e)  = do
  devwriteIO dev (encodeE e) `catch` ignore
  closeIO dev
  linfo Network $ printf "[fd: %s] session terminated with failure: %s" (show chan) (show e)
evalFinalizer chan dev (Right _)   = do
  closeIO dev
  linfo Network $ printf "[fd: %s] session terminated successfully" (show chan)

process :: (GraphBackend m) => m -> CoreServer -> Query -> IO Reply
process storage srv (Begin sig msg) =
  case (chkloads (parseLQL $ sigUser sig) msg) of
    Left _      -> return $ Fail 400 (Just "syntax error")
    Right stmts -> do
      (fh, dev) <- makeFD srv (sigUser sig)
      lnotice Network (printf "BEGIN %s %d" (show msg) fh)
      _         <- forkFinally (evalLQL storage srv dev stmts) (evalFinalizer fh dev)
      return $ Done fh
process _ srv (Fetch sig fh) = do
  let channel = (sigUser sig, fh)
  ldebug Network (printf "FETCH %d" fh)
  mdev <- selectFD srv channel
  case mdev of
    Nothing  -> return $ Fail 404 $ Just "no such channel"
    Just dev -> do
      blocks <- blkreadIO 32 dev
      case blocks of
        [] -> closeIO dev >> return (Last Nothing)
        _  -> let answer = foldr1 reduce blocks
              in when (isEOF answer) (closeIO dev) >> return answer
process _ srv (Close nowait sig fh) = do
  lnotice Network (printf "CLOSE %d" fh)
  closeFD srv nowait (sigUser sig, fh)
  return $ Last Nothing
