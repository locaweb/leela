{-# LANGUAGE OverloadedStrings #-}
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
       ( Asm(..)
       , Function(..)
       , ArithF(..)
       , isCreat
       , isFlush
       , isClose
       , isWrite
       ) where

import DarkMatter.Data.Time

-- | The functions available to users
data Function = Window Int Int
              | TimeWindow Time
              | Count
              | Mean
              | Median
              | Minimum
              | Maximum
              | Abs
              | Floor
              | Ceil
              | Round
              | Truncate
              | Arithmetic ArithF

data ArithF = Mul (Either Double Double)
            | Add (Either Double Double)
            | Div (Either Double Double)
            | Sub (Either Double Double)

-- | The available instructions to execute
data Asm = Write Int Time Double
         | Flush Int
         | Close Int
         | Creat Int [Function]

isCreat :: Asm -> Bool
isCreat (Creat _ _) = True
isCreat _           = False

isWrite :: Asm -> Bool
isWrite (Write _ _ _) = True
isWrite _             = False

isClose :: Asm -> Bool
isClose (Close _) = True
isClose _         = False

isFlush :: Asm -> Bool
isFlush (Flush _) = True
isFlush _         = False