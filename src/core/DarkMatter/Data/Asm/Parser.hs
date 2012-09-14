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

-- | The language that is used to communicate with the core. The
-- parser should be able to recognize the following grammar (ABNF):
-- 
--   S        = FETCH
--            / STORE
--            / PURGE
--            / WATCH
--   FETCH  = "fetch" KEY COL COL 1*XLIST
--   STORE  = "store" KEY COL VAL
--   THROW  = "throw" KEY VAL
--   PURGE  = "purge" KEY COL COL
--   WATCH  = "watch" KEY 1*XLIST
--   KEY    = DQUOTE 1*UTF8-CHAR DQUOTE
--   COL    = 1*DIGIT
--   XLIST  = "exec" PROC *("|" PROC)
--   VAL    = 1*DIGIT "." 1*DIGIT
--   PROC   = PURE 
--          / WINDOW
--          / "count"
--          / "mean"
--          / "median"
--          / "max"
--          / "min"
--   PURE   = "pure (" F ")"
--   WINDOW = "window" 1*DIGIT 1*DIGIT
--   F      = 1*DIGIT OP
--          / OP 1*DIGIT
--          / "ceil"
--          / "floor"
--          / "round"
--          / "truncate"
--          / "abs"
--          / "max" 1*DIGIT
--          / "min" 1*DIGIT
--   OP     = "*"
--          / "/"
--          / "+"
--          / "-"

module DarkMatter.Data.Asm.Parser
       ( compile
       ) where

import           Data.Attoparsec.Text as P
import qualified Data.Text as T
import           Data.Word
import           DarkMatter.Data.Asm.Types

compile :: T.Text -> Either String Asm
compile = parseOnly asmParser
  where asmParser = do { c <- peekChar
                       ; case c
                         of Just 's' -> parseStore
                            Just 't' -> parseThrow
                            Just 'f' -> parseFetch
                            Just 'w' -> parseWatch
                            Just 'p' -> parsePurge
                            _        -> fail "unknown instruction"
                       }

parseKey :: Parser T.Text
parseKey = do { _   <- char '"'
              ; key <- takeWhile1 (/='"')
              ; _   <- char '"'
              ; return key
              }

parseCol :: Parser Word32
parseCol = decimal

parseVal :: Parser Double
parseVal = double

parseStore :: Parser Asm
parseStore = do { _   <- string "store"
                ; skipSpace
                ; key <- parseKey
                ; skipSpace
                ; col <- parseCol
                ; skipSpace
                ; val <- parseVal
                ; return (Store key col val)
                }

parsePurge :: Parser Asm
parsePurge = do { _     <- string "purge"
                ; skipSpace
                ; key   <- parseKey
                ; skipSpace
                ; col_a <- parseCol
                ; skipSpace
                ; col_b <- parseCol
                ; return (Purge key (col_a,col_b))
                }

parseThrow :: Parser Asm
parseThrow = do { _     <- string "throw"
                ; skipSpace
                ; key   <- parseKey
                ; skipSpace
                ; val   <- parseVal
                ; return (Throw key val)
                }

parseWatch :: Parser Asm
parseWatch = do { _     <- string "watch"
                ; skipSpace
                ; _     <- parseKey
                ; return (Watch (const False) Nop)
                }

parseFetch :: Parser Asm
parseFetch = do { _    <- string "fetch"
                ; skipSpace
                ; key  <- parseKey
                ; skipSpace
                ; cola <- parseCol
                ; skipSpace
                ; colb <- parseCol
                ; return (Fetch key (cola,colb) Nop)
                }
