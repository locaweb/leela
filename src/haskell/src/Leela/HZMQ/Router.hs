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

module Leela.HZMQ.Router
       ( Worker (..)
       , Cfg (..)
       , defaultCfg
       , startRouter
       ) where

import           Data.Maybe
import           System.ZMQ3
import           Leela.Logger
import           Control.Monad
import           Leela.Helpers
import           Leela.Data.Time
import qualified Data.ByteString as B
import           Control.Exception
import           Control.Concurrent
import           Data.List.NonEmpty
import           Leela.Data.QDevice
import           Leela.HZMQ.ZHelpers

data Cfg = Cfg { endpoint     :: String
               , queueSize    :: Int
               , capabilities :: Int
               }

defaultCfg :: Cfg
defaultCfg = Cfg "tcp://*:4080" 32 16

data Request = Request Time B.ByteString [B.ByteString]

data Worker = Worker { onJob :: [B.ByteString] -> IO [B.ByteString]
                     , onErr :: SomeException -> IO [B.ByteString]
                     }

readMsg :: Request -> [B.ByteString]
readMsg (Request _ _ val) = val

readPeer :: Request -> B.ByteString
readPeer (Request _ val _) = val

reqTime :: Request -> Time
reqTime (Request val _ _) = val

logresult :: Request -> Maybe SomeException -> IO ()
logresult job me = do
  elapsed <- fmap (`diff` (reqTime job)) now
  linfo HZMQ $ printf "%s (%.4fms)" (failOrSucc me) (1000 * toDouble elapsed)
    where
      failOrSucc :: Maybe SomeException -> String
      failOrSucc Nothing  = "ROUTER.ok"
      failOrSucc (Just e) = printf "ROUTER.fail[%s]" (show e)

-- request :: Device Request -> Request -> IO ()
-- request queue req = devwriteIO queue req

-- dequeueReq :: Device Request -> IO Request
-- dequeueReq queue = fmap fromJust $ devreadIO queue

reply :: Request -> Socket Push -> [B.ByteString] -> IO ()
reply job fh msg = sendMulti fh (fromList $ readPeer job : "" : msg)

worker :: Request -> Socket Push -> Worker -> IO ()
worker job fh action = do
  mmsg <- try (onJob action (readMsg job))
  case mmsg of
    Left e    -> do
      logresult job (Just e)
      msg <- onErr action e
      reply job fh msg
    Right msg -> do
      logresult job Nothing
      reply job fh msg

forkWorker :: Context -> String -> Request -> Worker -> IO ()
forkWorker ctx addr job action = forkIO (do
  withSocket ctx Push $ \fh -> do
    connect fh addr
    configure fh
    worker job fh action) >> return ()

recvRequest :: Receiver a => Socket a -> IO (Maybe Request)
recvRequest fh = do
  mmsg <- receiveMulti fh
  time <- now
  case mmsg of
    (peer:"":msg) -> return $ Just (Request time peer msg)
    _             -> return Nothing

startRouter :: Control -> String -> Cfg -> Worker -> Context -> IO ()
startRouter ctrl name cfg action ctx = do
  lnotice HZMQ $
    printf "starting zmq.router: %s [qsize: %d, capabilities: %d, endpoint: %s]"
           name
           (queueSize cfg)
           (capabilities cfg)
           (endpoint cfg)
  withSocket ctx Router $ \ifh -> do
    withSocket ctx Pull $ \ofh -> do
      bind ofh oaddr
      bind ifh (endpoint cfg)
      configure ifh
      configure ofh
      superviseWith (fmap not $ closed ctrl) name (routingLoop ifh ofh)

    where
      oaddr = printf "inproc://%s.hzmq-router" name
       
      procRequest fh = do
        mreq <- recvRequest fh
        when (isJust mreq) (forkWorker ctx oaddr (fromJust mreq) action)
       
      routingLoop :: Socket Router -> Socket Pull -> IO ()
      routingLoop ifh ofh = do
        [eifh, eofh] <- poll 1000 [Sock ifh [In] Nothing, Sock ofh [In] Nothing]
        when (not $ null eifh) (procRequest ifh)
        when (not $ null eofh) (fmap fromList (receiveMulti ofh) >>= sendMulti ifh)

