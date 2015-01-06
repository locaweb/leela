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

module Leela.MonadHelpers where

import Leela.Logger
import Control.Monad
import Control.Exception
import Control.Concurrent

supervise :: Logger -> String -> IO () -> IO ()
supervise syslog name io = io `catch` restart
    where
      restart e =
        case (fromException e) of
          Just ThreadKilled -> return ()
          _                 -> do
            warning syslog (printf "%s: supervised function has died, restarting: %s" name (show e))
            threadDelay (250 * 1000)
            supervise syslog name io

supervise_ :: IO () -> IO ()
supervise_ = supervise devNull ""

mapMaybeM :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeM f = go []
    where
      go acc []     = return acc
      go acc (a:as) = do
        mb <- f a
        case mb of
          Just b  -> go (b : acc) as
          Nothing -> go acc as

ignore :: SomeException -> IO ()
ignore _ = return ()

foreverWith :: IO Bool -> IO () -> IO ()
foreverWith check io = do
  ok <- check
  when ok (io >> foreverWith check io)
