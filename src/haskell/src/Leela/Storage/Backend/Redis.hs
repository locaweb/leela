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
import           Leela.Storage.KeyValue

type Password = B.ByteString

data Answer a = Value a
              | Failure
              | TxAbort

newtype AnswerT m a = AnswerT { runAnswerT :: m (Answer a) }

data RedisBackend = RedisBackend (MVar ()) (Pool Endpoint Connection)

redisOpen :: (a, a -> IO [Endpoint]) -> Password -> IO RedisBackend
redisOpen (a, f) password = do
  ctrl <- newEmptyMVar
  pool <- createPool onBegin onClose
  _    <- forkSupervised (isEmptyMVar ctrl) "redisCluster" (do
            f a >>= updatePool pool
            threadDelay (5 * 1000 * 1000))
  return $ RedisBackend ctrl pool
    where
      onClose _ conn = void (runRedis conn quit)

      onBegin (TCP host port _) =
        connect (ConnInfo { connectHost           = host
                          , connectPort           = PortNumber (fromIntegral port)
                          , connectAuth           = Just password
                          , connectMaxConnections = 64
                          , connectMaxIdleTime    = 600
                          })
      onBegin _                 = throwIO SystemExcept

redisClose :: RedisBackend -> IO ()
redisClose (RedisBackend ctrl pool) = do
  putMVar ctrl ()
  deletePool pool

hashSelector :: B.ByteString -> [Endpoint] -> Endpoint
hashSelector s e = e !! (hash s `mod` length e)

runRedisIO :: AnswerT Redis a -> Connection -> IO a
runRedisIO action conn = go (1 :: Int)
    where
      go n
        | n >= 5    = throwIO SystemExcept
        | otherwise = do
          value <- runRedis conn (runAnswerT action)
          case value of
            Value a -> return a
            Failure -> throwIO SystemExcept
            TxAbort -> do
              lwarn Storage (printf "retrying redis transaction: %d/5" n)
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

  select (RedisBackend _ pool) k = use pool (hashSelector k) (runRedisIO $ liftRedis (get k))

  update (RedisBackend _ pool) k f = use pool (hashSelector k) (runRedisIO transaction)
      where
        transaction = do
          _     <- liftRedis $ watch [k]
          value <- (liftIO . f =<< liftRedis (get k))
          _     <- liftTxRedis (multiExec (set k value))
          return value

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
