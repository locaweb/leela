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
import Control.Exception
import Control.Concurrent

supervise :: String -> IO () -> IO ()
supervise name io = mask $ \restore -> restore io `catch` restart
    where
      restart :: SomeException -> IO ()
      restart e = do linfo Global (printf "supervised thread [%s] has died [except: %s]" name (show e))
                     supervise name io

sleep :: Int -> IO ()
sleep s = threadDelay (s * 1000000)
