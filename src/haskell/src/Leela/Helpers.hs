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
