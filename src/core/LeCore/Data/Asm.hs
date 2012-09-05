-- All Rights Reserved.
--
--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at
--
--        http://www.apache.org/licenses/LICENSE-2.0
--
--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License.

-- | The language that is used to communicate with the core. The
-- parser should be able to recognize the following grammar (ABNF):
-- 
--   S        = FETCH
--            / STORE
--            / PURGE
--            / WATCH
--   FETCH  = "FETCH" KEY COL COL 1*XLIST
--   STORE  = "STORE" KEY COL VAL
--   PURGE  = "PURGE" KEY COL COL
--   WATCH  = "WATCH" KEY 1*XLIST
--   KEY    = DQUOTE 1*UTF8-CHAR DQUOTE
--   COL    = 1*DIGIT
--   XLIST  = "EXEC" PROC *("|" PROC)
--   VAL    = 1*DIGIT "." 1*DIGIT
--   PROC   = PURE | WINDOW
--   PURE   = "pure" "(" F  ")"
--   WINDOW = "window" 1*DIGIT CATA-F
--   F      = // any [available] function (Double -> Double)
--   CATA-F = // any [available] function ([Double] -> Double)
module LeCore.Data.Asm
       (
       ) where

import LeCore.Data.Proc
