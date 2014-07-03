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
import Leela.Data.Time
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
                       , optEndpoint     :: Endpoint
                       , optDebugLevel   :: Priority
                       , optRedisSecret  :: String
                       }

defaultOptions :: Options
defaultOptions = Options { optEndpoint     = TCP "*" 4080 ""
                         , optDebugLevel   = NOTICE
                         , optConsul       = "http://127.0.0.1:8500"
                         , optRedisSecret  = ""
                         , optPasswd       = "/etc/leela/passwd"
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
  _     <- forkIO $ supervise syslog "main/passwd" $ do
    current <- readIORef shmem
    passwd  <- fmap (maybe current id) (parseFile file)
    when (passwd /= current) $ do
      warning syslog "loading new passwd file"
      writeIORef shmem passwd
    sleep 1
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
  resolver syslog naming (optConsul opts)
  _      <- forkIO (supervise syslog "main/resolver" $ sleep 5 >> resolver syslog naming (optConsul opts))
  void $ installHandler sigTERM (Catch $ signal alive) Nothing
  void $ installHandler sigINT (Catch $ signal alive) Nothing
  warning syslog
    (printf "warpdrive: yo!yo!; timeout=%d, endpoint=%s"
            (optTimeout opts)
            (show $ optEndpoint opts))
  withContext $ \ctx -> do
    let cfg = ClientConf (naming, fmap (maybe [] id . lookup "blackbox") . readIORef)
    cache  <- redisOpen syslog (naming, fmap (maybe [] id . lookup "redis") . readIORef) (optRedisSecret opts)
    client <- create syslog cfg ctx
    router <- startServer core (optEndpoint opts) ctx cache (zmqbackend client)
    takeMVar alive
    flushLogger syslog
    stopDealer client
    stopRouter router
  warning syslog "warpdrive: see ya!"
  closeLogger syslog
