{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Network.ZMQServer.Device
       ( Limit
       , Device (..)
       , expired
       , putTtl
       , decrttl
       , devwrite
       , trydevread
       , devread
       , blkread
       ) where

import Leela.Data.Excepts
import Control.Concurrent.STM
import Leela.Network.Protocol

type Limit = Int

data Device = RW Int (TBQueue Reply)
            | DevNull

expired :: Device -> Bool
expired DevNull  = True
expired (RW t _) = t <= 0

putTtl :: Int -> Device -> Device
putTtl _ DevNull  = DevNull
putTtl v (RW _ q) = RW v q

decrttl :: Device -> Device
decrttl DevNull    = DevNull
decrttl (RW ttl q) = RW (ttl - 1) q

devwrite :: Device -> Reply -> STM ()
devwrite DevNull _   = throwSTM BadDeviceExcept
devwrite (RW _ q) v  = writeTBQueue q v

trydevread :: Device -> STM (Maybe Reply)
trydevread DevNull  = return (Just NoSuchResourceError)
trydevread (RW _ q) = tryReadTBQueue q

devread :: Device -> STM Reply
devread DevNull  = return NoSuchResourceError
devread (RW _ q) = readTBQueue q

asReply :: [Reply] -> [Reply]
asReply [] = [TempUnavailError]
asReply xs = xs

blkread :: Limit -> Device -> STM [Reply]
blkread limit dev = blkread_ (max 0 limit) asReply
    where blkread_ 0 f = return (f [])
          blkread_ l f = do
            mmsg <- readfunc l
            case mmsg of
              Just msg
                | isChunk msg -> blkread_ (l - 1) (f . (msg :))
                | otherwise   -> blkread_ 0 (f . (msg :))
              Nothing         -> return (f [])
    
          readfunc l
              | l == limit = fmap Just (devread dev)
              | otherwise  = trydevread dev
                
