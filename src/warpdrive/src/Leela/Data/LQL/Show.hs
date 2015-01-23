{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Data.LQL.Show
       ( renderGrep
       ) where

import           Leela.Data.LQL
import           Leela.Data.Types
import qualified Data.ByteString.Lazy as L

renderGUID :: GUID -> L.ByteString
renderGUID (GUID g) = g

renderQStr :: L.ByteString -> L.ByteString
renderQStr = L.pack . fst . L.foldr escape ([], False)
    where
      escape w (acc, accept)
        | accept    = (w : acc, False)
        | w == 0x5c = (w : acc, True)
        | w == 0x22 = (0x5c : w : acc, False)
        | otherwise = (w : acc, False)

renderGrep :: Grep -> L.ByteString
renderGrep (GrepTAttr guid (Attr a))                = L.concat [ "grep attr "
                                                               , maybe "*" renderGUID guid
                                                               , " "
                                                               , renderQStr a
                                                               , " []"
                                                               ]        
renderGrep (GrepKAttr guid (Attr a))                = L.concat [ "grep attr "
                                                               , maybe "*" renderGUID guid
                                                               , " "
                                                               , renderQStr a
                                                               ]        
renderGrep (GrepMakeLink guidA (Label label) guidB) = L.concat [ "grep make "
                                                               , maybe "*" renderGUID guidA
                                                               , " "
                                                               , renderQStr label
                                                               , " "
                                                               , maybe "*" renderGUID guidB
                                                               ]        
renderGrep (GrepKillLink guidA (Label label) guidB) = L.concat [ "grep kill "
                                                               , maybe "*" renderGUID guidA
                                                               , " "
                                                               , renderQStr label
                                                               , " "
                                                               , maybe "*" renderGUID guidB
                                                               ]        
renderGrep (GrepMakeVertex (Kind k) (Node n))       = L.concat [ "grep make "
                                                               , renderQStr k
                                                               , " "
                                                               , renderQStr n
                                                               ]
renderGrep (GrepKillVertex mguid)                   = L.concat [ "grep kill "
                                                               , maybe "*" renderGUID mguid
                                                               ]
