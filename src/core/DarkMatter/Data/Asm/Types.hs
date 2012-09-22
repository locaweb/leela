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

module DarkMatter.Data.Asm.Types
       ( Key
       , Asm(..)
       , Function(..)
       , ArithOp(..)
       , isProc
       , isClose
       , isEvent
       ) where

import DarkMatter.Data.Time
import Data.ByteString as B

-- | The functions available to users
data Function = Window Int
              | TimeWindow Time
              | Sum
              | Prod
              | Mean
              | Median
              | Minimum
              | Maximum
              | Abs
              | Floor
              | Ceil
              | Round
              | Truncate
              | Arithmetic ArithOp Double

data ArithOp = Mul
             | Add
             | Div
             | Sub

type Key = B.ByteString

-- | The available instructions to execute
data Asm = Event Key Time Double
         | Proc [Function]
         | Close

isProc :: Asm -> Bool
isProc (Proc _) = True
isProc _        = False

isEvent :: Asm -> Bool
isEvent (Event _ _ _) = True
isEvent _             = False

isClose :: Asm -> Bool
isClose Close = True
isClose _     = False
