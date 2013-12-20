{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
--    
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--    
--     http://www.apache.org/licenses/LICENSE-2.0
--    
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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

zero :: Key
zero = pack L.empty

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
newline = void $ (string ", " <|> string "\n")

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

parseStmtStat :: Namespace -> Using -> Parser LQL
parseStmtStat n u
  | self u == system = "stat" .*> return (StatStmt u)
  | otherwise        = fail "stat statement must use the `system' namespace"
    where
      system = derive n ("system" :: L.ByteString)

parseStmtKill :: Using -> Parser LQL
parseStmtKill n = "kill " .*> doParse
    where
      doParse = do
        (a, ga) <- parseKeyAsGUID (self n)
        hardspace
        dl      <- parseLink (self n)
        hardspace
        (b, gb) <- parseKeyAsGUID (self n)
        case (b == zero, a == zero, dl) of
          (True, _, R l)  -> return $ KillStmt n (unlinkAll (ga, l))
          (False, _, R l) -> return $ KillStmt n (unlink (ga, gb, l))
          (_, True, L l)  -> return $ KillStmt n (unlinkAll (gb, l))
          (_, False, L l) -> return $ KillStmt n (unlink (gb, ga, l))
          (_, _, B l)     -> return $ KillStmt n (unlink (ga, gb, l) >> unlink (gb, ga, l))

parseStmt :: Namespace -> Using -> Parser LQL
parseStmt n u =
  parseStmtMake u
  <|> parseStmtPath u
  <|> parseStmtName u
  <|> parseStmtKill u
  <|> parseStmtStat n u

groupMakeStmts :: Using -> [LQL] -> [LQL]
groupMakeStmts u = partition (Nothing, [])
    where partition (Nothing, oStmt) []       = oStmt
          partition (Just cStmt, oStmt) []    = MakeStmt u cStmt : oStmt
          partition (cStmt, oStmt) (stmt:lql) =
            case stmt of
              MakeStmt _ r -> partition ((fmap (r >>) cStmt) `mplus` (Just r), oStmt) lql
              _            -> partition (cStmt, stmt : oStmt) lql

parseStmts :: Namespace -> Using -> Parser [LQL]
parseStmts n u = fmap (groupMakeStmts u) (parseStmt n u `sepBy1` newline)

parseUsing :: Namespace -> Parser Using
parseUsing user = do
  (treeN, treeK) <- "using " .*> parseNS user
  return (Using user (treeN, treeK))

parseLQL :: Namespace -> Parser [LQL]
parseLQL n = parseUsing n `endBy` separator >>= parseLQL1 n

parseLQL1 :: Namespace -> Using -> Parser [LQL]
parseLQL1 n u = parseStmts n u `endBy` semicolon

loads :: (Source i) => Parser a -> i -> Either String a
loads p = parseOnly p . bytestring

chkloads :: (Source i) => Parser a -> [i] -> Either String a
chkloads _ []     = Left "empty data"
chkloads p (x:xs) = go (parse p (bytestring x)) xs
    where
      go r []     = eitherResult (feed r B.empty)
      go r (i:is) = go (feed r (bytestring i)) is

instance Source B.ByteString where

  bytestring = id

instance Source String where

  bytestring = U.fromString

instance Source L.ByteString where

  bytestring = L.toStrict
