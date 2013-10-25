-- This file is part of Leela.
--
-- Leela is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Leela is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Leela.  If not, see <http://www.gnu.org/licenses/>.

module Leela.Config
       ( Cfg
       , Scope (..)
       -- * Parsers
       , asInt
       , asStr
       , fromStr
       , fromShow
       -- * Session
       , cfgOpen
       , cfgClose
       -- * Get
       , cfgGet
       , cfgGetM
       , cfgRead
       -- * Put
       , cfgWrite
       ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe
import           Data.Monoid
import           Leela.Logger
import           Data.Function
import           Leela.Helpers
import           Control.Monad
import qualified Data.ByteString as B
import           Control.Concurrent
import           Database.Zookeeper
import           Leela.Data.Endpoint
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import           Control.Concurrent.STM
import           Data.ByteString.Lazy.Builder

data Scope = Durable
           | Session
           | Local

timeout :: Int
timeout = 5000

type CfgEntry = M.Map String B.ByteString

data Cfg = Cfg { cfgEnv :: TVar (M.Map String (TVar CfgEntry))
               , cfgOut :: TMVar (String, [CreateFlag], B.ByteString)
               , cfgCtl :: TMVar ()
               } 
             deriving (Eq)

loadConf :: B.ByteString -> CfgEntry
loadConf b = M.fromList $ map splitPair (B.split 0x0a b)
    where
      ltrim = B.dropWhile (== 0x20)
      rtrim = B.reverse . ltrim . B.reverse

      splitPair s = let (l, r) = B.break (== 0x3d) s
                    in (B8.unpack $ rtrim l, ltrim $ B.drop 1 r)

dumpConf :: [(String, B.ByteString)] -> B.ByteString
dumpConf = L.toStrict . toLazyByteString . foldr (\v acc -> dumpValue v <> acc) mempty
    where
      dumpValue (k, v) = string7 k
                       <> string7 " = "
                       <> byteString v
                       <> char7 '\n'

readPath :: Zookeeper -> TVar CfgEntry -> IO () -> String -> IO ()
readPath zh cfg onDelete path = do
  rc <- exists zh path (Just watcher)
  case rc of
    Left NoNodeError -> do
      linfo Config (printf "readPath: node not found: %s" path)
      atomically $ writeTVar cfg M.empty
    Left invalid     -> lwarn Config (printf "readPath: zookeeper error %s: %s" path (show invalid))
    Right _          -> get zh path Nothing $ \result ->
      case result of
        Left NoNodeError   -> linfo Config (printf "readPath: node has vanished: %s" path)
        Left invalid       -> lwarn Config (printf "readPath: zookeeper error %s: %s" path (show invalid))
        Right (Nothing, _) -> do
          linfo Config (printf "readPath: node has no data: %s" path)
          atomically $ writeTVar cfg M.empty
        Right (Just b, _)  -> do
          linfo Config (printf "readPath: reloading data: %s" path)
          atomically $ writeTVar cfg (loadConf b)
    where
      watcher _ event ConnectedState _ =
        case event of
          DeletedEvent -> do
            lwarn Config (printf "readPath: node has vanished %s" path)
            atomically $ writeTVar cfg M.empty
            onDelete
          ChangedEvent -> readPath zh cfg onDelete path
          CreatedEvent -> readPath zh cfg onDelete path
          _            -> lwarn Config (printf "readPath: invalid event %s: %s" path (show event))
      watcher _ event state _          =
        lwarn Config (printf "readPath: invalid state/event %s: %s/%s" path (show event) (show state))

readLeaf :: Zookeeper -> TVar CfgEntry -> String -> IO ()
readLeaf zh tvar path = readPath zh tvar (readLeaf zh tvar path) path

readTree :: Zookeeper -> TVar (M.Map String (TVar CfgEntry)) -> String -> IO ()
readTree zh cfg path = newTVarIO S.empty >>= go
    where
      go seen = do
        rc <- exists zh path (Just dWatcher)
        case rc of
          Left NoNodeError -> do
            linfo Config (printf "readTree: node not found: %s" path)
            atomically $ writeTVar cfg M.empty
          Left invalid     -> lwarn Config (printf "getMany: zookeeper error %s: %s" path (show invalid))
          Right _          -> cWatcher seen zh ChildEvent ConnectedState Nothing

      difference seen list = atomically $ do
        old <- readTVar seen
        let new  = S.fromList list
            diff = new `S.difference` old
        writeTVar seen new
        return (S.toList diff)

      findTVar k = atomically $ do
        m <- readTVar cfg
        case (M.lookup k m) of
          Nothing   -> do
            tvar <- newTVar M.empty
            writeTVar cfg (M.insert k tvar m)
            return tvar
          Just tvar ->
            return tvar

      cWatcher seen _ ChildEvent ConnectedState _ =
        getChildren zh path (Just $ cWatcher seen) $ \result ->
          case result of
            Left NoNodeError -> linfo Config (printf "readTree: node has vanished: %s" path)
            Left invalid     -> lwarn Config (printf "readTree: zookepeer error %s: %s" path (show invalid))
            Right children   -> do
              unseen <- difference seen children
              linfo Config (printf "readTree: reading children %s: %s/%s" path (show unseen) (show children))
              forM_ unseen $ \child -> do
                let cPath = path ++ "/" ++ child
                tvar <- findTVar cPath
                readPath zh tvar (atomically $ modifyTVar cfg (M.delete child)) cPath
      cWatcher _ _ event state _
        | event /= DeletedEvent = lwarn Config (printf "readTree: invalid event/state %s: %s/%s" path (show event) (show state))
        | otherwise             = return ()

      dWatcher _ event ConnectedState _ =
        case event of
          DeletedEvent -> do
            lwarn Config (printf "readTree: node has vanished %s" path)
            newTVarIO S.empty >>= go
          CreatedEvent -> do
            lwarn Config (printf "readTree: node has been created %s " path)
            newTVarIO S.empty >>= go
          _            ->
            lwarn Config (printf "readTree: invalid event %s: %s" path (show event))
      dWatcher _ event state _ =
        lwarn Config (printf "readTree: invalid state/event %s: %s/%s" path (show event) (show state))

cfgOpen :: Endpoint -> [String] -> IO Cfg
cfgOpen endpoint paths = do
  zkGet <- newTVarIO M.empty
  zkPut <- newEmptyTMVarIO
  zkCtl <- newEmptyTMVarIO
  forkSupervised (atomically $ isEmptyTMVar zkCtl) "config.open" $ do
    lck <- newEmptyTMVarIO
    linfo Config "open: connecting to zookeeper..."
    withZookeeper (toZookeeper "localhost:2181" endpoint) timeout (Just $ watcher lck) Nothing $ \zh -> do
      forM_ paths $ \path -> do
        linfo Config "open: zookeeper connection established"
        tvar <- atomically $ do
          tmp <- newTVar M.empty
          modifyTVar zkGet (M.insertWith (\_ old -> old) path tmp)
          return tmp
        readLeaf zh tvar path
        readTree zh zkGet path
      fix $ \loop -> do
        event <- atomically $
          (fmap Right (takeTMVar zkPut))
            `orElse` (fmap Left (readTMVar lck))
              `orElse` (fmap Left (readTMVar zkCtl))
        case event of
          Left _                     -> return ()
          Right (path, flags, value) -> do
            execWrite zh path flags value
            loop
  return (Cfg zkGet zkPut zkCtl)
    where
      watcher mvar _ SessionEvent ExpiredSessionState _ = do
        lwarn Config (printf "conf/%s session has expired" (show paths))
        atomically $ putTMVar mvar ()
      watcher mvar _ SessionEvent AuthFailedState _     = do
        lwarn Config (printf "conf/%s authentication has failed" (show paths))
        atomically $ putTMVar mvar ()
      watcher _ _ event state _                         =
        lwarn Config (printf "conf/%s ignoring event/state %s/%s" (show paths) (show event) (show state))

      execWrite zh path flags value = do
        lwarn Config (printf "open: creating/updating node %s" path)
        rc <- exists zh path Nothing
        case rc of
          Left NoNodeError -> create zh path (Just value) OpenAclUnsafe flags (const $ return ())
          Left invalid     -> lwarn Config (printf "open: error stat node %s: %s" path (show invalid))
          Right stat       -> do
            myId <- getClientId zh
            mine <- ownsEphemeral myId stat
            if (Ephemeral `elem` flags && not mine)
              then do lwarn Config (printf "open: stale ephemeral node found, recreating %s" path)
                      void $ delete zh path (Just $ statVersion stat)
                      void $ create zh path (Just value) OpenAclUnsafe flags (const $ return ())
              else void $ set zh path (Just value) (Just $ statVersion stat)

cfgClose :: Cfg -> IO ()
cfgClose cfg = do
  lwarn Config "close: closing connection"
  atomically (void $ tryPutTMVar (cfgCtl cfg) ())
  threadDelay (timeout * 1000) -- allow zk to purge ephemeral nodes

cfgWrite :: Cfg -> Scope -> String -> [(String, B.ByteString)] -> IO ()
cfgWrite cfg Local path value = atomically $ do
  let mvalue = M.fromList value
  tvalue <- newTVar mvalue
  mentry <- fmap (M.lookup path) (readTVar (cfgEnv cfg))
  case mentry of
    Just entry -> modifyTVar entry (`M.union` mvalue)
    Nothing    -> modifyTVar (cfgEnv cfg) (M.insert path tvalue)
cfgWrite cfg scope path value =
  atomically $ putTMVar (cfgOut cfg) (path, flags, dumpConf value)
    where
      flags = case scope of
                Durable -> []
                Session -> [Ephemeral]
                _       -> error "the impossible happened!"

cfgRead :: Cfg -> (String -> Bool) -> IO [M.Map String B.ByteString]
cfgRead cfg selector = atomically $ do
  mNodes <- fmap (M.filterWithKey (\k _ -> selector k)) (readTVar (cfgEnv cfg))
  mapM readTVar (M.elems mNodes)

cfgGet :: (B.ByteString -> Maybe a) -> Cfg -> String -> String -> IO (Maybe a)
cfgGet parser cfg path key = fmap (cfgFind1 parser key) $ cfgRead cfg (== path)

cfgGetM :: (B.ByteString -> Maybe a) -> Cfg -> (String -> Bool) -> String -> IO [a]
cfgGetM parser cfg path key = fmap (cfgFind parser key) $ cfgRead cfg path

asInt :: B.ByteString -> Maybe Int
asInt s = case (B8.readInt s) of
            Just (n, rest)
              | B.null rest -> Just n
              | otherwise   -> Nothing
            Nothing         -> Nothing

asStr :: B.ByteString -> Maybe String
asStr = Just . B8.unpack

fromStr :: String -> B.ByteString
fromStr = B8.pack

fromShow :: Show a => a -> B.ByteString
fromShow = fromStr . show

cfgFind :: (B.ByteString -> Maybe a) -> String -> [M.Map String B.ByteString] -> [a]
cfgFind f k = mapMaybe ((>>= f) . M.lookup k)

cfgFind1 :: (B.ByteString -> Maybe a) -> String -> [M.Map String B.ByteString] -> Maybe a
cfgFind1 f k = safeHead . mapMaybe ((>>= f) . M.lookup k)
    where
      safeHead []    = Nothing
      safeHead (x:_) = Just x
