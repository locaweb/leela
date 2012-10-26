-- -*- mode: haskell; -*-
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
--   S        = PROC
--            / EVENT
--            / CLOSE
--   PROC     = "proc" SP PIPELINE EOL
--   PIPELINE = FUNC *(SP "|" SP FUNC)
--   EVENT    = "event" SP KEY SP TIME SP VAL EOL
--   CLOSE    = "close" EOL
--   EOL      = ";"
--   WINDOW   = "window" SP 1*DIGIT 1*DIGIT
--   MAP      = "map"
--   KEY      = 1*DIGIT "|" ALPHANUM
--   TIME     = 1*DIGIT "." 1*DIGIT
--   VAL      =                                                               ; DOUBLE NUMBER (e.g.: 2.7, -2.3, 3.3e7)
--   FUNC     = SFUNC
--            / AFUNC
--   SFUNC    = "(" AOP SP VAL ")"
--            / "(" VAL SP AOP ")"
--            / "id"
--            / "truncate"
--            / "ceil"
--            / "floor"
--            / "round"
--            / "abs"
--            / "mean"
--            / "median"
--            / "maximum"
--            / "mininmum"
--            / "sum"
--            / "prod"
--   AFUNC    = "window" SP 1*DIGIT SP "(" SFUNC *(SP "|" SP SFUNC) ")"
--            / "sma" SP 1*DIGIT
--            / "sample" SP 1*DIGIT SP 1*DIGIT
--            / "[" LOP SP VAL "]"
--            / "[" ROP SP VAL "]"
--   AOP      = "*"
--            / "/"
--            / "+"
--            / "-"
--   LOP      = ">"
--            / "<"
--            / "="
--            / ">="
--            / "<="
module DarkMatter.Data.Asm.Parser
       ( runOne
       , runAll
       , asmParser
       , eventParser
       ) where

import           Control.Monad
import           Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import           Text.Regex.TDFA
import           Text.Regex.TDFA.ByteString
import qualified Data.ByteString as B
import           DarkMatter.Data.Asm.Types
import qualified DarkMatter.Data.Event as E
import           DarkMatter.Data.Time

nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0

asmParser :: Parser Asm
asmParser = do { mc <- P8.peekChar
               ; case mc
                 of Just 'e' -> parseEvent
                    Just 'p' -> parseProc
                    Just 'c' -> parseClose
                    _        -> fail ("error: e|c|p were expected")
               }

eventParser :: Parser (Key, E.Event)
eventParser = fmap cast parseEvent
  where cast (Event k t d) = (k, E.temporal t d)
        cast _             = error "eventParser: event was expected"

endBy :: Parser a -> Parser () -> Parser a
endBy m s = do { r <- m
               ; _ <- s
               ; return r
               }

runOne :: Parser a -> B.ByteString -> Maybe a
runOne p i = either (const Nothing) Just (parseOnly (p `endBy` eol) i)

runAll :: Parser a -> B.ByteString -> [a]
runAll p i = either (const []) id (parseOnly parser i)
  where parser = do { eof <- atEnd
                    ; if eof
                      then return []
                      else liftM2 (:) (p `endBy` eol) parser
                    }

eol :: Parser ()
eol = P8.char ';' >> return ()

parseInt :: Parser Int
parseInt = P8.decimal

parseStr :: Parser B.ByteString
parseStr = do { n <- parseInt
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
parseVal = do { c <- P8.peekChar
              ; case c
                of Just 'n' -> string "nan"  >> return nan
                   Just 'i' -> string "inf"  >> return inf
                   Just '-' -> P8.char '-' >> fmap negate parseVal
                   _        -> choice [P8.double, P8.rational]
              }

parseClose :: Parser Asm
parseClose = P8.string "close" >> return Close

parseEvent :: Parser Asm
parseEvent = do { _   <- string "event "
                ; key <- parseStr
                ; _   <- P8.space
                ; col <- parseTime
                ; _   <- P8.space
                ; val <- parseVal
                ; return (Event key col val)
                }

parseMatch :: Parser Mode
parseMatch = do { _   <- string "match "
                ; str <- parseStr
                ; case (compile copts xopts str)
                  of Left err -> fail $ "parseMatch: " ++ err
                     Right r  -> return $ Match (str, matchTest r)
                }
  where copts = defaultCompOpt { lastStarGreedy = True  }
        xopts = defaultExecOpt { captureGroups  = False }

parseMode :: Parser Mode
parseMode = do { mc <- P8.peekChar
               ; case mc
                 of -- Just 's' -> string "stream" >> return Stream
                    Just 'm' -> parseMatch
                    _        -> fail "parseMode: s|b were expected"
               }

parseProc :: Parser Asm
parseProc = do { _         <- string "proc "
               ; mode      <- parseMode
               ; _         <- P8.space
               ; pipeline  <- parseFunction `sepBy` string " | "
               ; return (Proc mode pipeline)
               }

parseWindow :: Parser AsyncFunc
parseWindow = do { _ <- string "window "
                 ; n <- parseInt
                 ; _ <- P8.space
                 ; _ <- P8.char '('
                 ; p <- parseSyncFunc `sepBy` string " | "
                 ; _ <- P8.char ')'
                 ; if (n > 0)
                   then return (Window n p)
                   else fail "parseWindow: n <= 0"
                 }

parseSyncFunc :: Parser SyncFunc
parseSyncFunc = do { c <- P8.peekChar
                   ; case c
                     of Just 'a' -> string "abs" >> return Abs
                        Just 'c' -> string "ceil" >> return Ceil
                        Just 'f' -> string "floor" >> return Floor
                        Just 'i' -> string "id" >> return Id
                        Just 'm' -> choice [ string "mean"     >> return Mean
                                           , string "median"   >> return Median
                                           , string "minimum"  >> return Minimum
                                           , string "maximum"  >> return Maximum
                                           ]
                        Just 'p' -> string "prod" >> return Prod
                        Just 'r' -> string "round" >> return Round
                        Just 's' -> string "sum"  >> return Sum
                        Just 't' -> string "truncate" >> return Truncate
                        Just '(' -> parseArithmetic
                        _        -> fail "error: a|c|f|m|p|r|s|t|( were expected"
                   }

parseAsyncFunc :: Parser AsyncFunc
parseAsyncFunc = do { mc <- P8.peekChar
                    ; case mc
                      of Just 's' -> choice [ parseSMA
                                            , parseSample
                                            ]
                         Just 'w' -> parseWindow
                         Just '[' -> parseComparison
                         _        -> fail "parseAsyncFunc: s|w were expected"
                    }

parseFunction :: Parser Function
parseFunction = do { c <- P8.peekChar
                   ; case c
                     of Just 's' -> choice [ fmap Left parseAsyncFunc
                                           , fmap Right parseSyncFunc
                                           ]
                        Just 'w' -> fmap Left parseAsyncFunc
                        Just '[' -> fmap Left parseAsyncFunc
                        _        -> fmap Right parseSyncFunc
                   }

parseSMA :: Parser AsyncFunc
parseSMA = do { _ <- string "sma "
              ; n <- parseInt
              ; if (n > 0)
                then return (SMA n)
                else fail "parseSMA: n <= 0"
              }

parseSample :: Parser AsyncFunc
parseSample = do { _ <- string "sample "
                 ; n <- parseInt
                 ; _ <- P8.char '/'
                 ; m <- parseInt
                 ; if (n > 0 && m >= n)
                   then return (Sample n m)
                   else fail "parseSample: n <= 0 || m < n"
                 }

parseAOp :: Parser ArithOp
parseAOp = do { c <- P8.satisfy (`elem` "*+/-")
              ; case c
                of '*' -> return Mul
                   '+' -> return Add
                   '/' -> return Div
                   '-' -> return Sub
                   _   -> fail "error: *|+|-|/ were expected"
              }

parseLOp :: Parser ComparisonOp
parseLOp = do { c <- P8.satisfy (`elem` "><=/")
              ; case c
                of '>' -> ifNxEqOrElse (P8.char '=' >> return Ge) (return Gt)
                   '<' -> ifNxEqOrElse (P8.char '=' >> return Le) (return Lt)
                   '=' -> return Eq
                   '/' -> P8.char '=' >> return Ne
                   _   -> fail "error: >|<|= were expected"
              }
  where ifNxEqOrElse a b = do { isEq <- fmap (== Just '=') P8.peekChar
                              ; if (isEq)
                                then a
                                else b
                              }

parseArithL :: Parser SyncFunc
parseArithL = do { o <- parseAOp
                 ; _ <- P8.space
                 ; v <- parseVal
                 ; return (ArithmeticL o v)
                 }

parseArithR :: Parser SyncFunc
parseArithR = do { v <- parseVal
                 ; _ <- P8.space
                 ; o <- parseAOp
                 ; return (ArithmeticR o v)
                 }

parseArith :: Parser SyncFunc
parseArith = do { mc <- P8.peekChar
                ; case mc
                  of Just c 
                       | c `elem` "*/" -> parseArithL
                       | c `elem` "+-" -> choice [ parseArithL
                                                 , parseArithR
                                                 ]
                       | otherwise     -> parseArithR
                     Nothing  -> fail "parseArithmetic: unknow expression"
                }

parseCompareL :: Parser AsyncFunc
parseCompareL = do { o <- parseLOp
                   ; _ <- P8.space
                   ; v <- parseVal
                   ; return (ComparisonL o v)
                   }

parseCompareR :: Parser AsyncFunc
parseCompareR = do { v <- parseVal
                   ; _ <- P8.space
                   ; o <- parseLOp
                   ; return (ComparisonR o v)
                   }

parseCompare :: Parser AsyncFunc
parseCompare = do { mc <- P8.peekChar
                  ; case mc
                    of Just c 
                         | c `elem` "><=" -> parseCompareL
                         | otherwise      -> parseCompareR
                       Nothing  -> fail "parseCompare: unknow expression"
                  }

parseArithmetic :: Parser SyncFunc
parseArithmetic = do { _ <- P8.char '('
                     ; a <- parseArith
                     ; _ <- P8.char ')'
                     ; return a
                     }

parseComparison :: Parser AsyncFunc
parseComparison = do { _ <- P8.char '['
                     ; l <- parseCompare
                     ; _ <- P8.char ']'
                     ; return l
                     }
