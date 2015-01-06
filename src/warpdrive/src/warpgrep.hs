{-# LANGUAGE OverloadedStrings #-} -- TODO:remove

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

module Main (main) where

import Data.Word
import System.IO
import System.ZMQ4
import Leela.Logger
import Control.Monad
import Leela.HZMQ.Pipe
import Control.Concurrent
import System.Environment
import Leela.Data.Endpoint
import Leela.HZMQ.ZHelpers
import System.Posix.Signals
import System.Console.GetOpt
import Leela.Network.WarpGrepServer
import Leela.Data.LQL -- TODO:remove
import Leela.Data.Time -- TODO:remove
import Leela.Data.Types -- TODO:remove

data Options = Options { optEndpoint     :: Endpoint
                       , optBusEndpoint  :: Endpoint
                       , optDebugLevel   :: Priority
                       , optBufSize      :: Word
                       , optIoThreads    :: Word
                       , optLogFile      :: Maybe String
                       }

defaultOptions :: Options
defaultOptions = Options { optEndpoint     = TCP "*" 8040 ""
                         , optBusEndpoint  = TCP "*" 6969 ""
                         , optDebugLevel   = NOTICE
                         , optBufSize      = fromIntegral defaultBufSize
                         , optIoThreads    = 1
                         , optLogFile      = Nothing
                         }

setReadOpt :: (Read a) => (a -> Options -> Options) -> String -> Options -> Options
setReadOpt f raw opts = case (reads raw) of
                          [(a, "")] -> f a opts
                          _         -> error $ "error parsing: " ++ raw

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["endpoint"]
           (ReqArg (setReadOpt (\v opts -> opts { optEndpoint = v })) "ENDPOINT")
           (printf "endpoint to bind this service to [default: %s]" (show $ optEndpoint defaultOptions))
  , Option [] ["debug-level"]
           (ReqArg (setReadOpt (\v opts -> opts { optDebugLevel = v })) "DEBUG|INFO|NOTICE|WARNING|ERROR")
           (printf "logging level [default: %s]" (show $ optDebugLevel defaultOptions))
  , Option [] ["bus-endpoint"]
           (ReqArg (setReadOpt (\v opts -> opts { optBusEndpoint = v })) "BUS-ENDPOINT")
           (printf "the endpoint to bind the broadcast service to [default: %s]" (show $ optBusEndpoint defaultOptions))
  , Option [] ["log-bufsize"]
           (ReqArg (setReadOpt (\v opts -> opts { optBufSize = v })) "LOG-BUFSIZE")
           (printf "size of the buffer log [default: %d]" (optBufSize defaultOptions))
  , Option [] ["iothreads"]
           (ReqArg (setReadOpt (\v opts -> opts { optIoThreads = v })) "IOTHREADS")
           (printf "number of iothreads to use [default: %d]" (optIoThreads defaultOptions))
  , Option [] ["log-file"]
           (ReqArg (\v opts -> opts { optLogFile = Just v }) "LOG-FILE")
           (printf "path for the log file [default: %s]" (maybe "-" id $ optLogFile defaultOptions))
  ]

readOpts :: [String] -> IO Options
readOpts argv =
  case (getOpt Permute options argv) of
    (opts, _, []) -> return $ foldl (flip id) defaultOptions opts
    (_, _, errs)  -> ioError (userError (concat errs ++ usageInfo "usage: warpgrep [OPTION...]" options))

signal :: MVar () -> IO ()
signal x = void $ tryPutMVar x ()

zmqPubSocket :: Context -> IO (Socket Pub)
zmqPubSocket ctx = do
  fh <- socket ctx Pub
  setHWM (1000, 1000) fh
  return fh

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  opts    <- getArgs >>= readOpts
  alive   <- newEmptyMVar
  syslog  <- newLogger (optLogFile opts) (fromIntegral $ max 1 (optBufSize opts)) (optDebugLevel opts)
  void $ installHandler sigTERM (Catch $ signal alive) Nothing
  void $ installHandler sigINT (Catch $ signal alive) Nothing
  void $ installHandler sigHUP (Catch $ reopen syslog) Nothing
  warning syslog (printf "warpgrep: yo!yo!; endpoint=%s; bus-endpoint=%s" (show $ optEndpoint opts) (show $ optBusEndpoint opts))
  withContext $ \ctx -> do
    setIoThreads (optIoThreads opts) ctx
    pub     <- zmqPubSocket ctx
    warpsrv <- newWarpGrepServer syslog (socketAdapter pub)
    pipe    <- warpGrepServer warpsrv ctx
    configAndBind pub (dumpEndpointStr $ optBusEndpoint opts)
    pipeBind pipe (optEndpoint opts)
    forkIO (forever $ registerOrRefresh warpsrv (GrepKAttr Nothing (Attr "audit")) >> sleep 30) -- TODO:remove 
    takeMVar alive
    stopPipe pipe
    flushLogger syslog
  warning syslog "warpgrep: see ya!"
  closeLogger syslog
