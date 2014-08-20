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
import Leela.Network.WarpServer
import Leela.Storage.Backend.ZMQ
import Leela.Storage.Backend.Redis

data Options = Options { optConsul       :: Endpoint
                       , optPasswd       :: String
                       , optEndpoint     :: Endpoint
                       , optDebugLevel   :: Priority
                       , optRedisSecret  :: Maybe String
                       , optBufSize      :: Word
                       , optIoThreads    :: Word
                       }

defaultOptions :: Options
defaultOptions = Options { optEndpoint     = TCP "*" 4080 ""
                         , optDebugLevel   = NOTICE
                         , optConsul       = HTTP "127.0.0.1" 8500 ""
                         , optRedisSecret  = Nothing
                         , optPasswd       = "/etc/leela/passwd"
                         , optBufSize      = 512
                         , optIoThreads    = 2
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
  , Option [] ["consul-endpoint"]
           (ReqArg (setReadOpt (\v opts -> opts { optConsul = v })) "CONSUL-ENDPOINT")
           (printf "the consul endpoint to find leela services [default: %s]" (show $ optConsul defaultOptions))
  , Option [] ["debug-level"]
           (ReqArg (setReadOpt (\v opts -> opts { optDebugLevel = v })) "DEBUG|INFO|NOTICE|WARNING|ERROR")
           (printf "logging level [default: %s]" (show $ optDebugLevel defaultOptions))
  , Option [] ["redis-secret"]
           (ReqArg (\v opts -> opts { optRedisSecret = Just v }) "REDIS-SECRET")
           (printf "redis authentication string [default: %s]" (maybe "<<nopasswd>>" id (optRedisSecret defaultOptions)))
  , Option [] ["log-bufsize"]
           (ReqArg (setReadOpt (\v opts -> opts { optBufSize = v })) "LOG-BUFSIZE")
           (printf "size of the buffer log [default: %d]" (optBufSize defaultOptions))
  , Option [] ["passwd"]
           (ReqArg (\v opts -> opts { optPasswd = v }) "PASSWD")
           (printf "passwd file path [default: %s]" (optPasswd defaultOptions))
  , Option [] ["iothreads"]
           (ReqArg (setReadOpt (\v opts -> opts { optIoThreads = v })) "IOTHREADS")
           (printf "number of iothreads to use [default: %d]" (optIoThreads defaultOptions))
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
  _     <- forkIO $ supervise syslog "main/passwd" $ forever $ do
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
  syslog <- newLogger (fromIntegral $ max 1 (optBufSize opts)) (optDebugLevel opts)
  passwd <- passwdWatcher syslog (optPasswd opts)
  core   <- newCore syslog naming passwd
  resolver syslog naming (show $ optConsul opts)
  _      <- forkIO (supervise syslog "main/resolver" $ forever $ sleep 5 >> resolver syslog naming (show $ optConsul opts))
  void $ installHandler sigTERM (Catch $ signal alive) Nothing
  void $ installHandler sigINT (Catch $ signal alive) Nothing
  warning syslog
    (printf "warpdrive: yo!yo!; endpoint=%s" (show $ optEndpoint opts))
  withContext $ \ctx -> do
    let cfg     = ClientConf (naming, fmap (maybe [] id . lookup "blackbox") . readIORef)
        redisRW = fmap (maybe [] id . lookup "redis") . readIORef
        redisRO = fmap (maybe [] (map (portMap (+1))) . lookup "redis") . readIORef
    setIoThreads (optIoThreads opts) ctx
    cache  <- redisOpen syslog (naming, redisRO, redisRW) (optRedisSecret opts)
    client <- create syslog cfg ctx
    router <- warpServer core (optEndpoint opts) ctx (zmqbackend syslog client cache)
    takeMVar alive
    flushLogger syslog
    stopDealer client
    stopRouter router
  warning syslog "warpdrive: see ya!"
  closeLogger syslog
