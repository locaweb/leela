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

type Function = Either AsyncFunc SyncFunc

data SyncFunc = Sum
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
              | ArithmeticL ArithOp Double
              | ArithmeticR ArithOp Double
              deriving (Eq)

data AsyncFunc = Window Int [SyncFunc]
               | SMA Int
               | Sample Int Int
               | ComparisonL ComparisonOp Double
               | ComparisonR ComparisonOp Double
               deriving (Eq)

data ArithOp = Mul
             | Add
             | Div
             | Sub
             deriving (Eq)

data ComparisonOp = Lt
                  | Gt
                  | Eq
                  | Ge
                  | Le
                  | Ne
                  deriving (Eq)

type Key = B.ByteString

data Mode = Match (B.ByteString, Key -> Bool)
          | Stream

-- | The available instructions to execute
data Asm = Event Key Time Double
         | Proc Mode [Either AsyncFunc SyncFunc]
         | Close
         deriving (Eq)

isProc :: Asm -> Bool
isProc (Proc _ _) = True
isProc _          = False

isEvent :: Asm -> Bool
isEvent (Event _ _ _) = True
isEvent _             = False

isClose :: Asm -> Bool
isClose Close = True
isClose _     = False

instance Eq Mode where

  Stream    == Stream    = True
  (Match x) == (Match y) = fst x == fst y
  _         == _         = False