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
       , ArithmeticF(..)
       , Range
       , isStore
       , isThrow
       , isWatch
       , isFetch
       , isPurge
       ) where

import qualified Data.Text as T
import           Data.Word

type Range = (Word32, Word32)

-- | The functions available to users
data Function = Window Int Int
              | Mean
              | Median
              | Minimum
              | Maximum
              | Abs
              | Arithmetic ArithmeticF

data ArithmeticF = Mul (Either Double Double)
                 | Add (Either Double Double)
                 | Div (Either Double Double)
                 | Sub (Either Double Double)

-- | The available instructions to execute
data Asm = Store T.Text Word32 Double
           -- ^ Tells the engine to store this event
         | Fetch T.Text Range [Function]
           -- ^ Tells the engine to fetch this event
         | Watch T.Text [Function]
           -- ^ Monitors real time events with a set of functions
         | Purge T.Text Range
           -- ^ Removes an event from the storage backend
         | Throw T.Text Double
           -- ^ Tells the engine to process the event but not store it

isStore :: Asm -> Bool
isStore (Store _ _ _) = True
isStore _             = False

isFetch :: Asm -> Bool
isFetch (Fetch _ _ _) = True
isFetch _             = False

isWatch :: Asm -> Bool
isWatch (Watch _ _) = True
isWatch _           = False

isPurge :: Asm -> Bool
isPurge (Purge _ _) = True
isPurge _           = False

isThrow :: Asm -> Bool
isThrow (Throw _ _) = True
isThrow _           = False