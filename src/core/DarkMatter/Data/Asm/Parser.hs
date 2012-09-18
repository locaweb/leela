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
--   S        = CREAT
--            / EVENT
--            / FLUSH
--   EVENT  = "event" KEY TIME VAL
--   FLUSH  = "flush"
--   CREAT  = "creat" PROC *("|" PROC)
--   KEY    = 1*DIGIT
--   TIME   = 1*DIGIT "." 1*DIGIT
--   VAL    = 1*DIGIT "." 1*DIGIT
--   PROC   = BINF
--          / WINDOW
--          / "count"
--          / "truncate"
--          / "ceil"
--          / "floor"
--          / "round"
--          / "abs"
--          / "mean"
--          / "median"
--          / "maximum"
--          / "mininmum"
--   BINF   = (" F ")"
--   WINDOW = "window" 1*DIGIT 1*DIGIT
--   F      = 1*DIGIT OP
--          / OP 1*DIGIT
--   OP     = "*"
--          / "/"
--          / "+"
--          / "-"
module DarkMatter.Data.Asm.Parser
       ( parse
       , asmParser
       ) where

import           Data.Attoparsec.Char8 hiding (parse)
import qualified Data.ByteString.Char8 as B
import           DarkMatter.Data.Asm.Types
import           DarkMatter.Data.Time

asmParser :: Parser Asm
asmParser = do { r <- choice [ parseCreat
                             , parseFlush
                             , parseData
                             ]
               ; return r
               }

parse :: B.ByteString -> Either String Asm
parse = parseOnly asmParser

parseInt :: Parser Int
parseInt = decimal

parseKey :: Parser B.ByteString
parseKey = do { _ <- char '"'
              ; k <- takeWhile1 (/= '"')
              ; _ <- char '"'
              ; return k
              }

parseTime :: Parser Time
parseTime = do { s <- decimal
               ; n <- option 0 (char '.' >> decimal)
               ; return (mktime s n)
               }

parseVal :: Parser Double
parseVal = double

parseFlush :: Parser Asm
parseFlush = "flush" .*> return Flush

parseData :: Parser Asm
parseData = do { _  <- string "data"
               ; skipSpace
               ; key <- parseKey
               ; skipSpace
               ; col <- parseTime
               ; skipSpace
               ; val <- parseVal
               ; return (Data key col val)
               }

parseCreat :: Parser Asm
parseCreat = do { _         <- string "creat"
                ; skipSpace
                ; pipeline  <- parsePipeline
                ; return (Creat pipeline)
                }

parsePipeline :: Parser [Function]
parsePipeline = option [] (pipeSep >> parseFunction `sepBy1` pipeSep)
  where pipeSep = skipSpace >> char '|' >> skipSpace

parseFunction :: Parser Function
parseFunction = choice [ "mean"     .*> return Mean
                       , "median"   .*> return Median
                       , "minimum"  .*> return Minimum
                       , "maximum"  .*> return Maximum
                       , "count"    .*> return Count
                       , "truncate" .*> return Truncate
                       , "floor"    .*> return Floor
                       , "ceil"     .*> return Ceil
                       , "round"    .*> return Round
                       , "abs"      .*> return Abs
                       , parseWindow
                       , parseTimeWindow
                       , parseArithmetic
                       ]

parseArithF :: Parser ArithF
parseArithF = choice [ parseLeft
                     , parseRight
                     ]
  where parseLeft = choice [ "* " .*> fmap (Mul . Left) parseVal
                           , "/ " .*> fmap (Div . Left) parseVal
                           , "+ " .*> fmap (Add . Left) parseVal
                           , "- " .*> fmap (Sub . Left) parseVal
                           ]
        parseRight = fmap Right parseVal >>= \n -> choice [ " *" .*> return (Mul n)
                                                          , " /" .*> return (Div n)
                                                          , " +" .*> return (Add n)
                                                          , " -" .*> return (Sub n)
                                                          ]

parseWindow :: Parser Function
parseWindow = do { _ <- string "window"
                 ; skipSpace
                 ; n <- parseInt
                 ; skipSpace
                 ; m <- parseInt
                 ; return (Window n m)
                 }

parseTimeWindow :: Parser Function
parseTimeWindow = do { _ <- string "time_window"
                     ; skipSpace
                     ; fmap TimeWindow parseTime
                     }

parseArithmetic :: Parser Function
parseArithmetic = do { _ <- char '('
                     ; f <- parseArithF
                     ; _ <- char ')'
                     ; return (Arithmetic f)
                     }
