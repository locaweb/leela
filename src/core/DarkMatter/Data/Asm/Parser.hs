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
--   S      = PROC
--          / EVENT
--          / CLOSE
--   PROC   = "proc" SP FUNC *(SP "|" SP FUNC) EOL
--   EVENT  = "event" SP KEY SP TIME SP VAL EOL
--   CLOSE  = "close" EOL
--   EOL    = ";"
--   KEY    = 1*DIGIT "|" ALPHANUM
--   TIME   = 1*DIGIT "." 1*DIGIT
--   VAL    = 1*DIGIT "." 1*DIGIT
--   FUNC   = "(" ARITHF ")"
--          / WINDOW
--          / "sum"
--          / "prod"
--          / "truncate"
--          / "ceil"
--          / "floor"
--          / "round"
--          / "abs"
--          / "mean"
--          / "median"
--          / "maximum"
--          / "mininmum"
--   WINDOW = "window" SP 1*DIGIT SP 1*DIGIT
--   ARITHF = OP SP VAL
--   OP     = "*"
--          / "/"
--          / "+"
--          / "-"
module DarkMatter.Data.Asm.Parser
       ( runOne
       , runAll
       , asmParser
       ) where

import           Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.ByteString as B
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Time

asmParser :: Parser Asm
asmParser = do { mc <- P8.peekChar
               ; case mc
                 of Just 'e' -> parseEvent
                    Just 'p' -> parseProc
                    Just 'c' -> parseClose
                    _        -> fail ("error: e|c|p was expected")
               }

runOne :: B.ByteString -> Maybe (Asm, B.ByteString)
runOne i = case (parse asmParser i)
           of Done t asm -> Just (asm, t)
              _          -> Nothing

runAll :: B.ByteString -> [Asm]
runAll i = case (runOne i)
           of Just (asm, i1) -> asm : runAll i1
              Nothing        -> []

eol :: Parser ()
eol = P8.char ';' >> return ()

parseInt :: Parser Int
parseInt = P8.decimal

parseKey :: Parser B.ByteString
parseKey = do { n <- parseInt
              ; _ <- P8.char '|'
              ; P.take n
              }

parseTime :: Parser Time
parseTime = do { s <- parseInt
               ; _ <- P8.char '.'
               ; n <- parseInt
               ; return (mktime s n)
               }

parseVal :: Parser Double
parseVal = P8.double

parseClose :: Parser Asm
parseClose = P8.string "close" >> eol >> return Close

parseEvent :: Parser Asm
parseEvent = do { _   <- string "event "
                ; key <- parseKey
                ; _   <- P8.space
                ; col <- parseTime
                ; _   <- P8.space
                ; val <- parseVal
                ; eol
                ; return (Event key col val)
                }

parseProc :: Parser Asm
parseProc = do { _         <- string "proc "
               ; pipeline  <- parsePipeline
               ; eol
               ; return (Proc pipeline)
               }

parsePipeline :: Parser [Function]
parsePipeline = parseFunction `sepBy` pipeSep
  where pipeSep = P8.space >> P8.char '|' >> P8.space

parseFunction :: Parser Function
parseFunction = do { c <- P8.peekChar
                   ; case c
                     of Just 'a' -> string "abs" >> return Abs
                        Just 'c' -> string "ceil" >> return Ceil
                        Just 'f' -> string "floor" >> return Floor
                        Just 'm' -> choice [ string "mean"     >> return Mean
                                           , string "median"   >> return Median
                                           , string "minimum"  >> return Minimum
                                           , string "maximum"  >> return Maximum
                                           ]
                        Just 'p' -> string "prod" >> return Prod
                        Just 'r' -> string "round" >> return Round
                        Just 's' -> string "sum" >> return Sum
                        Just 't' -> choice [ string "truncate" >> return Truncate
                                           , parseTimeWindow
                                           ]
                        Just 'w' -> parseWindow
                        Just '(' -> parseArithmetic
                        _        -> fail "error: a|c|f|m|p|r|s|t|w|( was expected"
                   }

parseOp :: Parser ArithOp
parseOp = do { c <- P8.satisfy (`elem` "*+/-")
             ; case c
               of '*' -> return Mul
                  '+' -> return Add
                  '/' -> return Div
                  '-' -> return Sub
                  _   -> fail "error: *|+|-|/ was expected"
             }

parseWindow :: Parser Function
parseWindow = do { _ <- string "window "
                 ; n <- parseInt
                 ; _ <- P8.string " | "
                 ; f <- parsePipeline
                 ; return (Window n f)
                 }

parseTimeWindow :: Parser Function
parseTimeWindow = do { _ <- string "time_window "
                     ; fmap TimeWindow parseTime
                     }

parseArithmetic :: Parser Function
parseArithmetic = do { _ <- P8.char '('
                     ; o <- parseOp
                     ; _ <- P8.space
                     ; v <- parseVal
                     ; _ <- P8.char ')'
                     ; return (Arithmetic o v)
                     }
