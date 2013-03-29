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

module DarkMatter.Data.Parsers.Helpers where

import           Control.Monad
import           Data.Monoid ((<>))
import           Data.Double.Conversion.ByteString
import           Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           DarkMatter.Data.Event
import           DarkMatter.Data.Time

renderList :: (a -> Builder) -> [a] -> Builder
renderList f = foldr1 (<>) . map f

toString :: Builder -> String
toString = B8.unpack . toByteString

renderDouble :: Double -> Builder
renderDouble = fromByteString . toShortest

renderTime :: Time -> Builder
renderTime t = fromShow (seconds t)
               <> fromChar '.'
               <> fromShow (nseconds t)

renderStr :: B.ByteString -> Builder
renderStr k = fromShow (B.length k)
              <> fromString "|"
              <> fromByteString k

renderEvent :: Builder -> Event -> Builder
renderEvent k e = fromString "event "
                  <> k
                  <> fromChar ' '
                  <> renderTime (time e)
                  <> fromChar ' '
                  <> renderDouble (val e)
                  <> fromChar ';'

runOne :: Parser a -> B.ByteString -> Maybe a
runOne p i = either (const Nothing) Just (parseOnly (p `endBy` eol) i)

runAll :: Parser a -> B.ByteString -> [a]
runAll p i = either (const []) id (parseOnly parser i)
  where parser = do { eof <- atEnd
                    ; if eof
                      then return []
                      else liftM2 (:) (p `endBy` eol) parser
                    }

endBy :: Parser a -> Parser () -> Parser a
endBy m s = do { r <- m
               ; _ <- s
               ; return r
               }

eol :: Parser ()
eol = P8.char ';' >> return ()

nan :: Double
nan = 0 / 0

inf :: Double
inf = 1 / 0

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
                of Just 'n' -> string "nan" >> return nan
                   Just 'i' -> string "inf" >> return inf
                   Just '-' -> P8.char '-' >> fmap negate parseVal
                   _        -> P8.double
              }
