{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Storage.Backend.Redis
    ( Password
    , RedisBackend ()
    , redisOpen
    , redisClose
    , runRedisIO
    , runRedisIO_
    ) where

import           Data.Maybe
import           Leela.Logger
import           Control.Monad
import           Data.Hashable
import           Database.Redis as Redis
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Pool
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Excepts
import           Leela.MonadHelpers
import           Control.Applicative
import           Control.Monad.Trans
import           Leela.Data.Endpoint
import           Data.ByteString.Char8 (pack)
import           Leela.Storage.KeyValue
import           Control.Concurrent.Async

type Password = String

type MaxRetries = Int

data Answer a = Value a
              | Failure String
              | TxAbort
              deriving (Show)

newtype AnswerT m a = AnswerT { runAnswerT :: m (Answer a) }

data RedisBackend = RedisBackend Logger (MVar ()) (Pool Endpoint Connection) (Pool Endpoint Connection)

redisOpen :: Logger -> (IO [Endpoint], IO [Endpoint]) -> Maybe Password -> IO RedisBackend
redisOpen syslog (ronly, rdwr) password = do
  ctrl   <- newEmptyMVar
  ropool <- createPool onBegin onClose
  rwpool <- createPool onBegin onClose
  _      <- forkIO (supervise syslog "redis/pool" $ forever $ do
              ronly >>= updatePool ropool
              rdwr >>= updatePool rwpool
              sleep 1)
  return $ RedisBackend syslog ctrl ropool rwpool
    where
      onClose _ conn = void (runRedis conn quit)

      onBegin (TCP host port _) =
        connect (ConnInfo { connectHost           = host 
                          , connectPort           = PortNumber (fromIntegral port)
                          , connectAuth           = fmap pack password
                          , connectDatabase       = 0
                          , connectMaxConnections = 128
                          , connectMaxIdleTime    = 600})
      onBegin e                 = throwIO (SystemExcept (Just $ "Redis/redisOpen: invalid redis endpoint: " ++ (dumpEndpointStr e)))

redisClose :: RedisBackend -> IO ()
redisClose (RedisBackend _ ctrl ropool rwpool) = do
  putMVar ctrl ()
  deletePool ropool
  deletePool rwpool

hashSelector :: Hashable k => k -> [Endpoint] -> Endpoint
hashSelector s  e = e !! (hash s `mod` length e)

runRedisIO_ :: Logger -> AnswerT Redis a -> Connection -> IO a
runRedisIO_ syslog = runRedisIO syslog 0

runRedisIO :: Logger -> MaxRetries -> AnswerT Redis a -> Connection -> IO a
runRedisIO syslog maxretry action conn = go 0
    where
      go n
        | n > maxretry = throwIO (SystemExcept (Just "Redis/runRedisIO: error retrying transaction"))
        | otherwise    = do
          value <- runRedis conn (runAnswerT action)
          case value of
            Value a   -> return a
            Failure s -> throwIO (SystemExcept (Just $ "Redis/runRedisIO: error executing transaction: " ++ s))
            TxAbort   -> do
              warning syslog (printf "runRedisIO: retrying redis transaction: %d/5" n)
              threadDelay (n * 10 * 1000)
              go (n + 1)

liftRedis :: (Show a) => Redis (Either a b) -> AnswerT Redis b
liftRedis = AnswerT . liftM (either (Failure . show) Value)

liftTxRedis :: Redis (TxResult a) -> AnswerT Redis a
liftTxRedis m = AnswerT $ do
   ans <- m
   case ans of
     TxSuccess a -> return (Value a)
     TxAborted   -> return TxAbort
     TxError e   -> return (Failure e)

scanKeys :: Logger -> B.ByteString -> (Maybe [(B.ByteString, B.ByteString)] -> IO ()) -> Connection -> IO Bool
scanKeys syslog glob callback conn = go "0"
    where
      continue cursor
        | cursor == "0" = return True
        | otherwise     = go cursor

      fetchKeys [] cursor     = continue cursor
      fetchKeys keyset cursor = do
        let (lkeys, rkeys) = splitAt 2048 keyset
        mvalset <- runRedis conn $ mget lkeys
        case mvalset of
          Left what    -> do
            warning syslog ("scanKeys# error reading redis: " ++ show what)
            return False
          Right valset -> do
            callback (Just $ map (\(k, v) -> (k, fromJust v)) $ filter (isJust . snd) (zip lkeys valset))
            fetchKeys rkeys cursor

      go :: B.ByteString -> IO Bool
      go cursor = do
        mkeyset <- runRedis conn $ sendRequest ["SCAN", cursor, "MATCH", glob, "COUNT", "10000"]
        case mkeyset of
          Left what                  -> do
            warning syslog ("scanKeys# error reading redis: " ++ show what)
            return False
          Right (nextCursor, keyset) -> fetchKeys keyset nextCursor

instance KeyValue RedisBackend where

  exists (RedisBackend _ _ _ rwpool) sel k = use rwpool (hashSelector sel) (`runRedis` action)
      where
        action = liftM (either (const False) id) (Redis.exists k)

  select (RedisBackend _ _ _ rwpool) selector k   = use rwpool (hashSelector selector) (`runRedis` action)
      where
        action = liftM (either (const Nothing) id) (get k)

  insert (RedisBackend _ _ _ rwpool) ttl_ selector k v = use rwpool (hashSelector selector) (`runRedis` action)
      where
        action = liftM (== (Right Ok)) (setex k (fromIntegral ttl_) v)

  update (RedisBackend syslog _ _ rwpool) ttl_ selector k f = use rwpool (hashSelector selector) (runRedisIO syslog 3 transaction)
      where
        transaction = do
          _  <- liftRedis $ watch [k]
          v  <- (liftIO . f =<< liftRedis (get k))
          _  <- liftTxRedis (multiExec (psetex k (fromIntegral ttl_) v))
          return v

  scanOn (RedisBackend syslog _ ropool _) selector glob callback =
    use ropool (hashSelector selector) (void . scanKeys syslog glob callback)

  scanAll (RedisBackend syslog _ ropool _) glob callback =
    useAll ropool action
      where
        action conns = do
          results <- mapConcurrently (scanKeys syslog glob callback) conns
          unless (and results) (callback Nothing)

instance (Monad m) => Monad (AnswerT m) where

  return  = AnswerT . return . Value

  fail m  = AnswerT (return $ Failure m)

  a >>= f = AnswerT $ do
    value <- runAnswerT a
    case value of
      Value b   -> runAnswerT $ f b
      Failure m -> return (Failure m)
      TxAbort   -> return TxAbort

instance (Functor m) => Functor (AnswerT m) where

  fmap f ans = AnswerT $ fmap g (runAnswerT ans)
      where
        g (Value v)   = Value (f v)
        g (Failure m) = Failure m
        g TxAbort     = TxAbort

instance (Applicative m) => Applicative (AnswerT m) where

  pure = AnswerT . pure . Value

  af <*> avalue = AnswerT (g <$> runAnswerT af <*> runAnswerT avalue)
      where
        g (Value f) (Value v) = Value (f v)
        g (Failure m) _       = Failure m
        g TxAbort _           = TxAbort
        g _ (Failure m)       = Failure m
        g _ TxAbort           = TxAbort

instance MonadTrans AnswerT where

  lift = AnswerT . liftM Value

instance (MonadIO m) => MonadIO (AnswerT m) where

  liftIO = lift . liftIO
