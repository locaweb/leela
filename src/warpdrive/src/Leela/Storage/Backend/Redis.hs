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
    ) where

import           Leela.Logger
import           Control.Monad
import           Data.Hashable
import           Leela.Helpers
import           Database.Redis
import           Leela.Data.Pool
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Leela.Data.Excepts
import           Control.Monad.Trans
import           Leela.Data.Endpoint
import           Data.ByteString.Char8 (pack)
import           Leela.Storage.KeyValue

type Password = String

data Answer a = Value a
              | Failure
              | TxAbort

newtype AnswerT m a = AnswerT { runAnswerT :: m (Answer a) }

data RedisBackend = RedisBackend Logger (MVar ()) (Pool Endpoint Connection)

redisOpen :: Logger -> (a, a -> IO [Endpoint]) -> Password -> IO RedisBackend
redisOpen syslog (a, f) password = do
  ctrl <- newEmptyMVar
  pool <- createPool onBegin onClose
  -- _    <- forkIO (do
  --           f a >>= updatePool pool
  --           sleep 1)
  return $ RedisBackend syslog ctrl pool
    where
      onClose _ conn = void (runRedis conn quit)

      onBegin (TCP host port _) =
        connect (ConnInfo { connectHost           = host
                          , connectPort           = PortNumber (fromIntegral port)
                          , connectAuth           = Just (pack password)
                          , connectDatabase       = 0
                          , connectMaxConnections = 64
                          , connectMaxIdleTime    = 600
                          })
      onBegin e                 = throwIO (SystemExcept (Just $ "Redis/redisOpen: invalid redis endpoint: " ++ (dumpEndpointStr e)))

redisClose :: RedisBackend -> IO ()
redisClose (RedisBackend _ ctrl pool) = do
  putMVar ctrl ()
  deletePool pool

hashSelector :: B.ByteString -> [Endpoint] -> Endpoint
hashSelector s e = e !! (hash s `mod` length e)

runRedisIO :: Logger -> AnswerT Redis a -> Connection -> IO a
runRedisIO syslog action conn = go (1 :: Int)
    where
      go n
        | n >= 5    = throwIO (SystemExcept (Just "Redis/runRedisIO: error executing transaction"))
        | otherwise = do
          value <- runRedis conn (runAnswerT action)
          case value of
            Value a -> return a
            Failure -> throwIO (SystemExcept (Just "Redis/runRedisIO: error executing transaction"))
            TxAbort -> do
              warning syslog (printf "retrying redis transaction: %d/5" n)
              threadDelay (n * 10 * 1000)
              go (n + 1)

liftRedis :: Redis (Either a b) -> AnswerT Redis b
liftRedis = AnswerT . liftM (either (const Failure) Value)

liftTxRedis :: Redis (TxResult a) -> AnswerT Redis a
liftTxRedis m = AnswerT $ do
   ans <- m
   case ans of
     TxSuccess a -> return (Value a)
     TxAborted   -> return TxAbort
     TxError _   -> return Failure

instance KeyValue RedisBackend where

  select (RedisBackend syslog _ pool) k   = use pool (hashSelector k) (runRedisIO syslog $ liftRedis (get k))

  update (RedisBackend syslog _ pool) k f = use pool (hashSelector k) (runRedisIO syslog transaction)
      where
        transaction = do
          _  <- liftRedis $ watch [k]
          v  <- (liftIO . f =<< liftRedis (get k))
          _  <- liftTxRedis (multiExec (set k v))
          return v

instance (Monad m) => Monad (AnswerT m) where

  return  = AnswerT . return . Value

  fail _  = AnswerT (return Failure)

  a >>= f = AnswerT $ do
    value <- runAnswerT a
    case value of
      Value b -> runAnswerT $ f b
      Failure -> return Failure
      TxAbort -> return TxAbort

instance MonadTrans AnswerT where

  lift = AnswerT . liftM Value

instance (MonadIO m) => MonadIO (AnswerT m) where

  liftIO = lift . liftIO
