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

import System.IO
import Data.IORef
import System.ZMQ4
import Leela.Logger
import Leela.Naming
import Control.Monad
import Leela.Helpers
import Leela.HZMQ.Dealer
import Control.Concurrent
import Leela.Network.Core
import System.Environment
import Leela.Data.Endpoint
import System.Posix.Signals
import Leela.Storage.Passwd
import System.Console.GetOpt
import Leela.Network.ZMQServer
import Leela.Storage.Backend.ZMQ
import Leela.Storage.Backend.Redis

data Options = Options { optConsul       :: String
                       , optPasswd       :: String
                       , optTimeout      :: Int
                       , optBacklog      :: Int
                       , optEndpoint     :: Endpoint
                       , optDebugLevel   :: Priority
                       , optRedisSecret  :: String
                       , optCapabilities :: Int
                       }

defaultOptions :: Options
defaultOptions = Options { optEndpoint     = TCP "*" 4080 ""
                         , optDebugLevel   = NOTICE
                         , optConsul       = "http://127.0.0.1:8500"
                         , optRedisSecret  = ""
                         , optPasswd       = "/etc/leela/passwd"
                         , optBacklog      = 64
                         , optCapabilities = 8
                         , optTimeout      = 60 * 1000
                         }

setReadOpt :: (Read a) => (a -> Options -> Options) -> String -> Options -> Options
setReadOpt f raw opts = case (reads raw) of
                          [(a, "")] -> f a opts
                          _         -> error $ "error parsing: " ++ raw

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['e'] ["endpoint"]
           (ReqArg (setReadOpt (\v opts -> opts { optEndpoint = v })) "ENDPOINT")
           "endpoint to bind this service to"
  , Option [] ["consul-endpoint"]
           (ReqArg (\v opts -> opts { optConsul = v }) "CONSULENDPOINT")
           "the consul endpoint to find leela services"
  , Option [] ["debug-level"]
           (ReqArg (setReadOpt (\v opts -> opts { optDebugLevel = v })) "DEBUG|INFO|NOTICE|WARNING|ERROR")
           "logging level"
  , Option []    ["redis-secret"]
           (ReqArg (\v opts -> opts { optRedisSecret = v }) "REDISSECRET")
           "redis authentication string"
  , Option [] ["backlog"]
           (ReqArg (setReadOpt (\v opts -> opts { optBacklog = v })) "BACKLOG")
           "storage queue size"
  , Option [] ["capabilities"]
           (ReqArg (setReadOpt (\v opts -> opts { optCapabilities = v })) "CAPABILITIES")
           "number of threads per storage connection"
  , Option [] ["timeout-in-ms"]
           (ReqArg (setReadOpt (\v opts -> opts { optTimeout = v})) "TIMEOUT-IN-MS")
           "timeout in milliseconds"
  , Option [] ["passwd"]
           (ReqArg (\v opts -> opts { optPasswd = v }) "PASSWD")
           "passwd file path"
  ]

readOpts :: [String] -> IO Options
readOpts argv =
  case (getOpt Permute options argv) of
    (opts, _, []) -> return $ foldl (flip id) defaultOptions opts
    (_, _, errs)  -> ioError (userError (concat errs ++ usageInfo "usage: warpdrive [OPTION...]" options))

signal :: MVar () -> IO ()
signal x = tryPutMVar x () >> return ()

passwdWatcher :: Logger -> FilePath -> IO (IORef Passwd)
passwdWatcher syslog file = do
  shmem <- newIORef zero
  forkSupervised_ syslog "passwd watcher" (do
    current <- readIORef shmem
    passwd  <- fmap (maybe current id) (parseFile file)
    when (passwd /= current) (do
      warning syslog "loading new passwd file"
      writeIORef shmem passwd)
    threadDelay $ 5 * 1000 * 1000)
  return shmem

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  opts   <- getArgs >>= readOpts
  alive  <- newEmptyMVar
  naming <- newIORef []
  syslog <- newLogger (optDebugLevel opts)
  passwd <- passwdWatcher syslog (optPasswd opts)
  core   <- newCore syslog naming passwd
  void $ installHandler sigTERM (Catch $ signal alive) Nothing
  void $ installHandler sigINT (Catch $ signal alive) Nothing
  warning syslog
    (printf "warpdrive: starting; timeout=%d, backlog=%d caps=%d endpoint=%s"
            (optTimeout opts)
            (optBacklog opts)
            (optCapabilities opts)
            (show $ optEndpoint opts))
  forkSupervised_ syslog "resolver" $ resolver syslog (optEndpoint opts) naming (optConsul opts)
  withContext $ \ctx -> do
    setMaxSockets 64000 ctx
    let cfg = ClientConf (optBacklog opts)
                         (naming, fmap (maybe [] id . lookup "blackbox") . readIORef)
                         (optCapabilities opts)
    cache       <- redisOpen syslog (naming, fmap (maybe [] id . lookup "redis") . readIORef) (optRedisSecret opts)
    (client, _) <- create syslog cfg ctx
    void $ forkFinally (startServer core (optEndpoint opts) ctx cache (zmqbackend client)) $ \e -> do
      warning syslog (printf "warpdrive has died: %s" (show e))
      signal alive
    takeMVar alive
  warning syslog "warpdrive: bye!"
