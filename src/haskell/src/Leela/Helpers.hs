-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

module Leela.Helpers where

import Leela.Logger
import Control.Monad
import Control.Exception
import Control.Concurrent

forkSupervised :: IO Bool -> String -> IO () -> IO ()
forkSupervised check name io =
  void $ forkIO (superviseWith check name io)

forkSupervised_ :: String -> IO () -> IO ()
forkSupervised_ = forkSupervised (return True)

superviseWith :: IO Bool -> String -> IO () -> IO ()
superviseWith check name io =
  (foreverWith check io) `onException` restart
    where
      restart = do
        threadDelay 500000
        linfo Global (printf "supervised thread [%s] has died" name)
        superviseWith check name io

ignore :: SomeException -> IO ()
ignore _ = return ()

foreverWith :: IO Bool -> IO () -> IO ()
foreverWith check io = do
  ok <- check
  when ok (io >> foreverWith check io)

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000000)
