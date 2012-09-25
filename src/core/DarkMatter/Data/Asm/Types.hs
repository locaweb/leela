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

-- | Mostly this defines the types the parser module is able to
-- recognize.
module DarkMatter.Data.Asm.Types where

import DarkMatter.Data.Time
import Data.ByteString as B

-- | The mode the pipeline will be executed.
data Mode = Window Int Int
          -- ^ Defined a buffer of size N (first arg) and speed M
          --   (second arg).  In other words, invoke the pipeline when
          --   N items are read, discarding N-M items afterwards.
          | Passthrough
          -- ^ Runs without any buffering at all.
          deriving (Show)

data Function = Sum
              | Prod
              | Mean
              | Median
              | Minimum
              | Maximum
              | Id
              | Abs
              | Floor
              | Ceil
              | Round
              | Truncate
              | Arithmetic ArithOp Double
              deriving (Show)

data ArithOp = Mul
             | Add
             | Div
             | Sub
             deriving (Show)

type Key = B.ByteString

-- | The available instructions to execute
data Asm = Event Key Time Double
         | Proc Mode [Function]
         | Close
         deriving (Show)

isProc :: Asm -> Bool
isProc (Proc _ _) = True
isProc _          = False

isEvent :: Asm -> Bool
isEvent (Event _ _ _) = True
isEvent _             = False

isClose :: Asm -> Bool
isClose Close = True
isClose _     = False
