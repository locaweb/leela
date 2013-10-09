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

module Leela.HZMQ.ZHelpers where

import Data.Int
import System.ZMQ3
import Data.ByteString (ByteString)

recvTimeout :: (Receiver a) => Int64 -> Socket a -> IO (Maybe [ByteString])
recvTimeout t s = do
  ready <- poll t [Sock s [In] Nothing]
  case ready of
    [[]] -> return Nothing
    _    -> fmap Just (receiveMulti s)

ms :: Int -> Int
ms = (* 1000)

configure :: Socket a -> IO ()
configure fh = do
  setLinger (restrict (ms 0)) fh
  setReconnectInterval (restrict (ms 1)) fh
  setMaxMessageSize (restrict (1024*1024 :: Int)) fh
