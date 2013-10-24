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
import Leela.Data.Endpoint

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

toEndpoint :: Endpoint -> [String]
toEndpoint e
  | isTCP e   = map buildItem (eAddr e)
  | otherwise = ["tcp://localhost:4080"]
    where
      buildItem (h, Nothing) = "tcp://" ++ h ++ ":50021"
      buildItem (h, Just p)  = "tcp://" ++ h ++ ":" ++ show p

toEndpoint1 :: Endpoint -> String
toEndpoint1 = head . toEndpoint
