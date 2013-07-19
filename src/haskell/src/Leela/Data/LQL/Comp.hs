{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- This file is part of Leela.
--
-- Leela is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Leela is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Leela.  If not, see <http://www.gnu.org/licenses/>.

module Leela.Data.LQL.Comp
    ( Source (..)
    , parseLQL
    , parseLQL1
    , parseUsing
    , loads
    , chkloads
    ) where

import           Data.Word
import           Control.Monad
import           Leela.Data.LQL
import           Data.Attoparsec as A
import qualified Data.ByteString as B
import           Leela.Data.Graph as G
import           Control.Applicative
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as L
import           Leela.Data.Namespace as N
import           Data.Attoparsec.Char8 as A8

type NKey = (Key, L.ByteString)

type NLabel = (Label, L.ByteString)

data Direction a = L a
                 | R a
                 | B a

class Source a where

    bytestring :: a -> B.ByteString

hardspace :: Parser ()
hardspace = char ' ' >> return ()

parseKey :: Namespace -> Parser NKey
parseKey _ = do s <- qstring 128 0x28 0x29
                return (pack s, s)

parseKeyAsGUID :: Namespace -> Parser (Key, GUID)
parseKeyAsGUID n = do (k, _) <- parseKey n
                      return (k, guid $ derive n k)

parseNS :: Namespace -> Parser (Namespace, NKey)
parseNS n = do s <- qstring 128 0x28 0x29
               return (derive n s, (pack s, s))

parseLabel :: Namespace -> Parser NLabel
parseLabel _ = do s <- qstring 128 0x5b 0x5d
                  return (pack s, s)

qstring :: Int -> Word8 -> Word8 -> Parser L.ByteString
qstring limit l r = word8 l >> anyWord8 >>= loop limit []
    where loop lim acc x
              | x == r    = return (L.pack (reverse acc))
              | lim < 0   = fail "qstring:too long"
              | x == 0x5c = do c <- anyWord8
                               anyWord8 >>= loop (lim - 1) (c : acc)
              | otherwise = anyWord8 >>= loop (lim - 1) (x : acc)

newline :: Parser ()
newline = char '\n' >> return ()

semicolon :: Parser ()
semicolon = char ';' >> return ()

parseLink :: Namespace -> Parser (Direction NLabel)
parseLink n = do mdir <- char '-' <|> char '<'
                 l    <- option (pack L.empty, L.empty) (parseLabel n)
                 f    <- case mdir of
                           '-' -> (liftM (const B) (char '-')) <|> (liftM (const R) (char '>'))
                           '<' -> liftM (const L) (char '-')
                           _   -> fail "parseLink"
                 return (f l)

parseRLink :: Namespace -> Parser NLabel
parseRLink n = do dlink <- parseLink n
                  case dlink of
                    R l -> return l
                    _   -> fail "parseRLink: wrong direction"

parseG :: Namespace -> Parser (G.Result ())
parseG n = do (k, kg) <- parseKeyAsGUID n
              peekChar >>= fmap (putNode n k >>) . (go kg [])
  where go k g (Just ' ') = do l <- hardspace >> parseLink n
                               r <- hardspace >> fmap snd (parseKeyAsGUID n)
                               peekChar >>= go r (asLink g k r l)
        go _ g _          = return (putLinks g)

        asLink acc k r (L (l, _)) = (r, k, l) : acc
        asLink acc k r (R (l, _)) = (k, r, l) : acc
        asLink acc k r (B (l, _)) = (k, r, l) : (r, k, l) : acc

endBy :: Parser a -> Parser b -> Parser a
endBy = liftM2 const

parseQuery :: Namespace -> Parser Cursor
parseQuery n = do q0 <- fmap (select . snd) (parseKeyAsGUID n)
                  parseQuery1 n q0

parseQuery1 :: Namespace -> Cursor -> Parser Cursor
parseQuery1 n q = option q doParse
    where doParse = do hardspace
                       (_, sl) <- parseRLink n
                       hardspace
                       (k, sk) <- parseKey n
                       if (L.null sk)
                         then parseQuery1 n (outlabelWith (comparator sl) q)
                         else parseQuery1 n (outnodeWith (comparator sl) (== (guid $ derive n k)) q)

          comparator s = let isPrefix = "*" `L.isPrefixOf` s
                             isSuffix = "*" `L.isSuffixOf` s
                             left     = L.init s
                             right    = L.tail s
                             mid      = L.tail (L.init s)
                         in case (isPrefix, isSuffix) of
                              (True, True)   -> or . map (mid `L.isPrefixOf`) . L.tails . unpack
                              (True, False)  -> (right `L.isSuffixOf`) . unpack
                              (False, True)  -> (left `L.isPrefixOf`) . unpack
                              (False, False) -> (s ==) . unpack

parseStmtCreate :: Using -> Parser LQL
parseStmtCreate n = "create " .*> liftM (Create n) (parseG (self n))

parseStmtMatch :: Using -> Parser LQL
parseStmtMatch n = "match " .*> liftM (Match n) (parseQuery (self n))

hexAlphabet :: [Word8]
hexAlphabet = [0x78] ++ [0x30 .. 0x39] ++ [0x41 .. 0x46] ++ [0x61 .. 0x66]

parseStmtResolve :: Using -> Parser LQL
parseStmtResolve n = "resolve " .*> liftM (Resolve n . pack) (A.takeWhile (\c -> c `elem` hexAlphabet))

parseStmt :: Using -> Parser LQL
parseStmt n = parseStmtCreate n
              <|> parseStmtMatch n
              <|> parseStmtResolve n

parseStmts :: Using -> Parser [LQL]
parseStmts u = parseStmt u `sepBy1` (char '\n')

parseUsing :: Namespace -> Parser Using
parseUsing r = do (uns, (ukey, user)) <- "using " .*> parseNS r
                  (nns, (nkey, name)) <- hardspace >> parseNS uns
                  (akey, _)           <- option (ukey, user) parseAs
                  return (Using uns (nns, nkey, name) akey)
    where parseAs = " as " .*> parseKey r

parseLQL :: Namespace -> Parser [LQL]
parseLQL n = parseUsing n `endBy` newline >>= parseLQL1

parseLQL1 :: Using -> Parser [LQL]
parseLQL1 u = parseStmts u `endBy` semicolon

loads :: (Source i) => Parser a -> i -> Either String a
loads p = parseOnly p . bytestring

chkloads :: (Source i) => Parser a -> [i] -> Either String a
chkloads p = go (parse p B.empty)
    where go r []     = eitherResult r
          go r (i:is) = go (feed r (bytestring i)) is

instance Source B.ByteString where

    bytestring = id

instance Source String where

    bytestring = U.fromString

instance Source L.ByteString where

    bytestring = L.toStrict
