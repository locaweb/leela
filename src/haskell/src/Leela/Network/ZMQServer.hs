{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Network.ZMQServer
       ( ZMQServer
       , create
       , zrun
       ) where

import           System.ZMQ3
import           Control.Monad
import           Leela.Data.LQL
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import           Control.Exception
import           Leela.HZMQ.Router
import           Control.Concurrent
import           Leela.Data.Excepts
import           Leela.Storage.Backend
import           Control.Concurrent.STM
import           Leela.Network.Protocol as R
import           Leela.Network.CoreServer
import           Leela.Network.ZMQServer.Device
import           Leela.Network.ZMQServer.Protocol

data ZMQServer b = ZMQServer { backend :: b
                             , fdseq   :: TVar FH
                             , handles :: TVar (M.Map FH Device)
                             }

qsize :: Int
qsize = 32

defaultTtl :: Int
defaultTtl = 30

znextfd :: ZMQServer b -> STM FH
znextfd z = do
  k <- readTVar (fdseq z)
  writeTVar (fdseq z) (k + 1)
  return k

zbegin :: ZMQServer b -> IO FH
zbegin z =
  atomically $ do
    k <- znextfd z
    m <- readTVar (handles z)
    d <- fmap (RW defaultTtl) (newTBQueue qsize)
    writeTVar (handles z) (M.insert k d m)
    return k

rungc :: ZMQServer a -> IO [FH]
rungc z =
  atomically $ do
    (dead, alive) <- fmap (M.partition expired) (readTVar (handles z))
    when (M.size alive > 0) (writeTVar (handles z) (M.map decrttl alive))
    return (M.keys dead)

zclose :: ZMQServer a -> FH -> IO ()
zclose z fh = atomically (zalter (const Nothing) z fh)

zselect :: ZMQServer a -> FH -> STM Device
zselect z fh = fmap select (readTVar (handles z))
  where
    select m = maybe DevNull id (M.lookup fh m)

zalter :: (Maybe Device -> Maybe Device) -> ZMQServer a -> FH -> STM ()
zalter f z fh = modifyTVar' (handles z) (M.alter f fh)

zwrite :: ZMQServer a -> FH -> Reply -> IO ()
zwrite z fh r =
  atomically $ do
    dev <- zselect z fh
    devwrite dev r

zreaper :: ZMQServer a -> IO ()
zreaper z = rungc z >>= mapM_ (zclose z)

forkLQL :: (GraphBackend a) => ZMQServer a -> FH -> [LQL] -> IO ()
forkLQL z fh lql = forkFinally doWork cleaner >> return ()
  where
    doWork = perform (backend z) (zwrite z fh) lql

    cleaner (Left e) =
      case (fromException e) of
        Just BadDeviceExcept -> return ()
        _                    -> zwrite z fh (liftE e)
    cleaner  _       = return ()

worker :: (GraphBackend a) => ZMQServer a -> Worker
worker z = Worker (exec z . msgunpack) g
  where
    g e = return (msgpack1 (Data (liftE e)))

exec :: (GraphBackend a) => ZMQServer a -> Request -> IO [B.ByteString]
exec z (Fetch fh limit) = do
  atomically (zalter (liftM (putTtl defaultTtl)) z fh)
  fmap msgpack (atomically (zselect z fh >>= blkread limit))
exec z (Begin lql)      = do
  fh <- zbegin z
  forkLQL z fh lql
  return (msgpack1 (Channel fh))
exec z (Close fh)       = do
  zclose z (fromIntegral fh)
  return (msgpack1 done)

create :: a -> IO (ZMQServer a)
create a = liftM2 (ZMQServer a) (newTVarIO 0) (newTVarIO M.empty)

zrun :: (GraphBackend a) => ZMQServer a -> Context -> String -> IO ()
zrun z ctx addr = do
  _ <- forkIO (forever (threadDelay 1000000 >> zreaper z))
  start "zmqserver" (defaultCfg {endpoint = addr}) (worker z) ctx

