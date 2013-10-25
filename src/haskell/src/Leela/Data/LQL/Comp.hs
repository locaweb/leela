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

data Direction a = L a
                 | R a
                 | B a

class Source a where

    bytestring :: a -> B.ByteString

hardspace :: Parser ()
hardspace = void $ char ' '

parseKey :: Namespace -> Parser Key
parseKey _ = do
  s <- qstring 128 0x28 0x29
  return (pack s)

parseKeyAsGUID :: Namespace -> Parser (Key, GUID)
parseKeyAsGUID n = do
  k <- parseKey n
  return (k, guid $ derive n k)

parseNS :: Namespace -> Parser (Namespace, Key)
parseNS n = do
  s <- qstring 128 0x28 0x29
  return (derive n s, pack s)

parseLabel :: Namespace -> Parser Label
parseLabel _ = do
  s <- qstring 128 0x5b 0x5d
  return (pack s)

qstring :: Int -> Word8 -> Word8 -> Parser L.ByteString
qstring limit l r = word8 l >> anyWord8 >>= loop limit []
    where
      loop lim acc x
        | x == r    = return (L.pack (reverse acc))
        | lim < 0   = fail "qstring:too long"
        | x == 0x5c = do
            c <- anyWord8
            anyWord8 >>= loop (lim - 1) (c : acc)
        | otherwise = anyWord8 >>= loop (lim - 1) (x : acc)

newline :: Parser ()
newline = void $ char '\n'

separator :: Parser ()
separator = void (char '\n' <|> char ' ')

semicolon :: Parser ()
semicolon = void $ char ';'

parseLink :: Namespace -> Parser (Direction Label)
parseLink n = do
  mdir <- char '-' <|> char '<'
  l    <- option (pack L.empty) (parseLabel n)
  f    <- case mdir of
           '-' -> (liftM (const B) (char '-')) <|> (liftM (const R) (char '>'))
           '<' -> liftM (const L) (char '-')
           _   -> fail "parseLink"
  return (f l)

parseRLink :: Namespace -> Parser Label
parseRLink n = do
  dlink <- parseLink n
  case dlink of
    R l -> return l
    _   -> fail "parseRLink: wrong direction"

parseG :: Namespace -> Parser (G.Result ())
parseG n = do
  (k, kg) <- parseKeyAsGUID n
  peekChar >>= fmap (putNode n k >>) . (go kg [])
    where
      go k g (Just ' ') = do
        l <- hardspace >> parseLink n
        r <- hardspace >> fmap snd (parseKeyAsGUID n)
        peekChar >>= go r (asLink g k r l)
      go _ g _          = return (putLinks g)

      asLink acc k r (L l) = (r, k, l) : acc
      asLink acc k r (R l) = (k, r, l) : acc
      asLink acc k r (B l) = (k, r, l) : (r, k, l) : acc

endBy :: Parser a -> Parser b -> Parser a
endBy = liftM2 const

parseQuery :: Namespace -> Parser Cursor
parseQuery n = do
  q0 <- fmap (start . snd) (parseKeyAsGUID n)
  parseQuery1 n q0

parseQuery1 :: Namespace -> Cursor -> Parser Cursor
parseQuery1 n q = option q doParse
    where
      zero = pack L.empty

      doParse = do
        hardspace
        l <- parseRLink n
        hardspace
        (b, gb) <- parseKeyAsGUID n
        if (b == zero)
          then parseQuery1 n (select l Nothing q)
          else parseQuery1 n (select l (Just gb) q)

parseStmtMake :: Using -> Parser LQL
parseStmtMake n = "make " .*> liftM (MakeStmt n) (parseG (self n))

parseStmtPath :: Using -> Parser LQL
parseStmtPath n = "path " .*> liftM (PathStmt n) (parseQuery (self n))

hexAlphabet :: [Word8]
hexAlphabet = [0x78] ++ [0x30 .. 0x39] ++ [0x41 .. 0x46] ++ [0x61 .. 0x66]

parseStmtName :: Using -> Parser LQL
parseStmtName n = "name " .*> liftM (NameStmt n . pack) (A.takeWhile (`elem` hexAlphabet))

parseStmt :: Using -> Parser LQL
parseStmt n =
  parseStmtMake n
  <|> parseStmtPath n
  <|> parseStmtName n

groupMakeStmts :: Using -> [LQL] -> [LQL]
groupMakeStmts u = partition (Nothing, [])
    where partition (Nothing, oStmt) []       = oStmt
          partition (Just cStmt, oStmt) []    = MakeStmt u cStmt : oStmt
          partition (cStmt, oStmt) (stmt:lql) =
            case stmt of
              MakeStmt _ r -> partition ((fmap (r >>) cStmt) `mplus` (Just r), oStmt) lql
              _            -> partition (cStmt, stmt : oStmt) lql

parseStmts :: Using -> Parser [LQL]
parseStmts u = fmap (groupMakeStmts u) (parseStmt u `sepBy1` newline)

parseUsing :: Namespace -> Parser Using
parseUsing user = do
  (treeN, treeK) <- "using " .*> parseNS user
  return (Using user (treeN, treeK))

parseLQL :: Namespace -> Parser [LQL]
parseLQL n = parseUsing n `endBy` separator >>= parseLQL1

parseLQL1 :: Using -> Parser [LQL]
parseLQL1 u = parseStmts u `endBy` semicolon

loads :: (Source i) => Parser a -> i -> Either String a
loads p = parseOnly p . bytestring

chkloads :: (Source i) => Parser a -> [i] -> Either String a
chkloads p = go (parse p B.empty)
    where
      go r []     = eitherResult r
      go r (i:is) = go (feed r (bytestring i)) is

instance Source B.ByteString where

  bytestring = id

instance Source String where

  bytestring = U.fromString

instance Source L.ByteString where

  bytestring = L.toStrict
