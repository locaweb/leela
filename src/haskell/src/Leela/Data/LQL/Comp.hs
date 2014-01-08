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

hardspace :: Parser ()
hardspace = void $ word8 0x20

parseKey :: Parser Key
parseKey = do
  s <- qstring 128 0x28 0x29
  return (pack s)

parseNS :: Namespace -> Parser Namespace
parseNS n = do
  s <- qstring 128 0x28 0x29
  return $ derive n s

parseLabel :: Parser Label
parseLabel = do
  s <- qstring 128 0x5b 0x5d
  return (pack s)

parseGUID :: Parser GUID
parseGUID = liftM pack $ A.takeWhile (`elem` hexAlphabet)

parseMaybeGUID :: Parser (Maybe GUID)
parseMaybeGUID = ("()" .*> return Nothing) <|> (liftM Just parseGUID)

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

parseLink :: Parser (Direction Label)
parseLink = do
  mdir <- char '-' <|> char '<'
  l    <- option (pack L.empty) parseLabel
  f    <- case mdir of
           '-' -> (liftM (const B) (char '-')) <|> (liftM (const R) (char '>'))
           '<' -> liftM (const L) (char '-')
           _   -> fail "parseLink"
  return (f l)

parseRLink :: Parser Label
parseRLink = do
  dlink <- parseLink
  case dlink of
    R l -> return l
    _   -> fail "parseRLink: wrong direction"

parseMakeCreate :: Parser (G.Result ())
parseMakeCreate = do
  k <- parseGUID
  peekChar >>= go k []
    where
      go a g (Just ' ') = do
        l <- hardspace >> parseLink
        b <- hardspace >> parseGUID
        peekChar >>= go b (asLink g a b l)
      go _ g _          = return (putLinks g)

      asLink acc a b (L l) = (b, a, l) : acc
      asLink acc a b (R l) = (a, b, l) : acc
      asLink acc a b (B l) = (a, b, l) : (b, a, l) : acc

endBy :: Parser a -> Parser b -> Parser a
endBy = liftM2 const

parseQuery :: Parser Cursor
parseQuery = do
  q0 <- liftM start parseGUID
  parseQuery1 q0

parseQuery1 :: Cursor -> Parser Cursor
parseQuery1 q = option q doParse
    where
      doParse = do
        hardspace
        l <- parseRLink
        hardspace
        mgb <- parseMaybeGUID
        case mgb of
          Nothing -> parseQuery1 (select l Nothing q)
          Just gb -> parseQuery1 (select l (Just gb) q)

parseStmtMake :: Using -> Parser LQL
parseStmtMake u = do
  void $ string "make "
  at <- peekWord8
  case at of
    Just 0x30 -> liftM AlterStmt parseMakeCreate
    Just 0x28 -> liftM (AlterStmt . putNode (uTree u)) parseKey
    _         -> fail "bad make statement"

parseStmtPath :: Parser LQL
parseStmtPath = "path " .*> liftM PathStmt parseQuery

hexAlphabet :: [Word8]
hexAlphabet = [0x78] ++ [0x30 .. 0x39] ++ [0x41 .. 0x46] ++ [0x61 .. 0x66]

parseStmtName :: Using -> Parser LQL
parseStmtName u = "name " .*> liftM (NameStmt u) parseGUID

parseStmtGUID :: Using -> Parser LQL
parseStmtGUID u = "guid " .*> liftM (GUIDStmt u) parseKey

parseStmtStat :: Parser LQL
parseStmtStat = "stat" .*> return StatStmt

parseStmtKill :: Parser LQL
parseStmtKill = "kill " .*> doParse
    where
      doParse = do
        mga <- parseMaybeGUID
        hardspace
        dl  <- parseLink
        hardspace
        mgb <- parseMaybeGUID
        case (mga, mgb, dl) of
          (Just ga, Nothing, R l) -> return $ AlterStmt (unlinkAll (ga, l))
          (Just ga, Just gb, R l) -> return $ AlterStmt (unlink (ga, gb, l))
          (Nothing, Just gb, L l) -> return $ AlterStmt (unlinkAll (gb, l))
          (Just ga, Just gb, L l) -> return $ AlterStmt (unlink (gb, ga, l))
          (Just ga, Just gb, B l) -> return $ AlterStmt (unlink (ga, gb, l) >> unlink (gb, ga, l))
          _                       -> fail "invalid kill command"

parseStmt :: Using -> Parser LQL
parseStmt u =
  parseStmtMake u
  <|> parseStmtGUID u
  <|> parseStmtPath
  <|> parseStmtName u
  <|> parseStmtKill
  <|> parseStmtStat

parseStmts :: Using -> Parser [LQL]
parseStmts u = parseStmt u `sepBy1` newline

parseUsing :: Namespace -> Parser Using
parseUsing user = liftM (Using user) ("using " .*> parseNS user)

parseLQL :: Namespace -> Parser [LQL]
parseLQL n = parseUsing n `endBy` separator >>= parseLQL1

parseLQL1 :: Using -> Parser [LQL]
parseLQL1 u = parseStmts u `endBy` semicolon

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
