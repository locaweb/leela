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

import           Leela.Logger
import           Control.Monad
import           Data.Hashable
import           Leela.Helpers
import           Database.Redis as Redis
import           Leela.Data.Time
import           Leela.Data.Pool
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Excepts
import           Control.Applicative
import           Control.Monad.Trans
import           Leela.Data.Endpoint
import           Data.ByteString.Char8 (pack)
import           Leela.Storage.KeyValue

type Password = String

type MaxRetries = Int

data Answer a = Value a
              | Failure String
              | TxAbort
              deriving (Show)

newtype AnswerT m a = AnswerT { runAnswerT :: m (Answer a) }

data RedisBackend = RedisBackend Logger (MVar ()) (Pool Endpoint Connection)

redisOpen :: Logger -> (a, a -> IO [Endpoint]) -> Maybe Password -> IO RedisBackend
redisOpen syslog (a, f) password = do
  ctrl <- newEmptyMVar
  pool <- createPool onBegin onClose
  _    <- forkIO (supervise syslog "redis/pool" $ forever $ do
            f a >>= updatePool pool
            sleep 1)
  return $ RedisBackend syslog ctrl pool
    where
      onClose _ conn = void (runRedis conn quit)

      onBegin (TCP host port _) =
        connect (ConnInfo { connectHost           = host 
                          , connectPort           = PortNumber (fromIntegral port)
                          , connectAuth           = fmap pack password
                          , connectDatabase       = 0
                          , connectMaxConnections = 8
                          , connectMaxIdleTime    = 600})
      onBegin e                 = throwIO (SystemExcept (Just $ "Redis/redisOpen: invalid redis endpoint: " ++ (dumpEndpointStr e)))

redisClose :: RedisBackend -> IO ()
redisClose (RedisBackend _ ctrl pool) = do
  putMVar ctrl ()
  deletePool pool

hashSelector :: B.ByteString -> [Endpoint] -> Endpoint
hashSelector s e = e !! (hash s `mod` length e)

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

instance KeyValue RedisBackend where

  exists (RedisBackend _ _ pool) k   = use pool (hashSelector k) (\c -> runRedis c action)
      where
        action = liftM (either (const False) id) (Redis.exists k)

  select (RedisBackend _ _ pool) k   = use pool (hashSelector k) (\c -> runRedis c action)
      where
        action = liftM (either (const Nothing) id) (get k)

  insert (RedisBackend _ _ pool) ttl_ k v = use pool (hashSelector k) (\c -> runRedis c action)
      where
        action = liftM (== (Right Ok)) (setex k (fromIntegral ttl_) v)

  update (RedisBackend syslog _ pool) ttl_ k f = use pool (hashSelector k) (runRedisIO syslog 3 transaction)
      where
        transaction = do
          _  <- liftRedis $ watch [k]
          v  <- (liftIO . f =<< liftRedis (get k))
          _  <- liftTxRedis (multiExec (psetex k (fromIntegral ttl_) v))
          return v

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
