{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
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
import           Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import           Leela.Data.Graph as G
import           Leela.Data.Naming
import           Control.Applicative
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as L
import           Data.Attoparsec.Char8 ((.*>))

data Direction a = L a
                 | R a
                 | B a

class Source a where

    bytestring :: a -> B.ByteString

isSpace :: Parser Bool
isSpace = liftM (== Just 0x20) peekWord8

hardspace :: Parser ()
hardspace = void $ word8 0x20

parseNode :: Parser Node
parseNode = liftM Node (qstring 128 0x28 0x29)

parseTree :: Parser Tree
parseTree = liftM Tree (qstring 128 0x28 0x29)

parseLabel :: Parser Label
parseLabel = liftM Label (qstring 128 0x5b 0x5d)

parseGUID :: Parser GUID
parseGUID = liftM GUID (A.take 36)

parseMaybeGUID :: Parser (Maybe GUID)
parseMaybeGUID = ("()" .*> return Nothing) <|> (liftM Just parseGUID)

qstring :: Int -> Word8 -> Word8 -> Parser B.ByteString
qstring limit l r = word8 l >> anyWord8 >>= loop limit []
    where
      loop lim acc x
        | x == r    = return (B.pack (reverse acc))
        | lim < 0   = fail "qstring:too long"
        | x == 0x5c = do
            c <- anyWord8
            anyWord8 >>= loop (lim - 1) (c : acc)
        | otherwise = anyWord8 >>= loop (lim - 1) (x : acc)

newline :: Parser ()
newline = void $ ((word8 0x2c >> word8 0x20) <|> word8 0x0a)

separator :: Parser ()
separator = void (word8 0x0a <|> word8 0x20)

semicolon :: Parser ()
semicolon = void $ word8 0x3b

parseLink :: Parser (Direction Label)
parseLink = do
  mdir <- word8 0x2d <|> word8 0x3c
  l    <- option (Label B.empty) parseLabel
  f    <- case mdir of
           0x2d -> (liftM (const B) (word8 0x2d)) <|> (liftM (const R) (word8 0x3e))
           0x3c -> liftM (const L) (word8 0x2d)
           _    -> fail "parseLink"
  return (f l)

parseRLink :: Parser Label
parseRLink = do
  dlink <- parseLink
  case dlink of
    R l -> return l
    _   -> fail "parseRLink: wrong direction"

parseMakeCreate :: Parser [Journal]
parseMakeCreate = do
  k <- parseGUID
  peekWord8 >>= go k []
    where
      go a g (Just 0x20) = do
        l <- hardspace >> parseLink
        b <- hardspace >> parseGUID
        peekWord8 >>= go b (asLink g a b l)
      go _ g _           = return (concatMap (\(a, l, b) -> [PutLabel a l, PutLink a l b]) g)

      asLink acc a b (L l) = (b, l, a) : acc
      asLink acc a b (R l) = (a, l, b) : acc
      asLink acc a b (B l) = (a, l, b) : (b, l, a) : acc

endBy :: Parser a -> Parser b -> Parser a
endBy = liftM2 const

safeHead :: [a] -> Parser a
safeHead [] = fail "parsing error"
safeHead xs = return $ head xs

parseQuery :: Parser (Matcher, [(GUID -> Matcher)])
parseQuery = do
  a  <- parseGUID
  ok <- isSpace
  if ok
    then do
      q <- parseQuery1 []
      f <- safeHead q
      return (f a, tail q)
    else return (ByNode a, [])

parseQuery1 :: [(GUID -> Matcher)] -> Parser [(GUID -> Matcher)]
parseQuery1 acc = do
  ok <- isSpace
  if ok
    then do
      hardspace
      l  <- parseRLink
      hardspace
      mb <- parseMaybeGUID
      case mb of
        Nothing -> parseQuery1 ((flip ByLabel l) : acc)
        Just b  -> parseQuery1 ((\a -> ByEdge a l b) : acc)
    else
      return (reverse acc)

parseStmtMake :: Using -> Parser LQL
parseStmtMake u = do
  void $ string "make "
  at <- peekWord8
  case at of
    Just 0x28 -> liftM (AlterStmt . return . PutNode (uUser u) (uTree u)) parseNode
    Just _    -> liftM AlterStmt parseMakeCreate
    _         -> fail "bad make statement"

parseStmtPath :: Parser LQL
parseStmtPath = "path " .*> liftM PathStmt parseQuery

parseStmtName :: Using -> Parser LQL
parseStmtName u = "name " .*> liftM (NameStmt u) parseGUID

parseStmtGUID :: Using -> Parser LQL
parseStmtGUID u = "guid " .*> liftM (GUIDStmt u) parseNode

parseStmtStat :: Parser LQL
parseStmtStat = "stat" .*> return StatStmt

parseStmtKill :: Parser LQL
parseStmtKill = "kill " .*> doParse
    where
      doParse = do
        ma <- parseMaybeGUID
        hardspace
        dl  <- parseLink
        hardspace
        mb <- parseMaybeGUID
        case (ma, mb, dl) of
          (Just a, Nothing, R l) -> return $ AlterStmt [DelLink a l Nothing]
          (Just a, Just b, R l)  -> return $ AlterStmt [DelLink a l (Just b)]
          (Nothing, Just b, L l) -> return $ AlterStmt [DelLink b l Nothing]
          (Just a, Just b, L l)  -> return $ AlterStmt [DelLink b l (Just a)]
          (Just a, Just b, B l)  -> return $ AlterStmt [DelLink a l (Just b), DelLink b l (Just a)]
          _                      -> fail "invalid kill command"

parseStmt :: Using -> Parser LQL
parseStmt u = do
  w <- peekWord8
  case w of
    Just 0x6d -> parseStmtMake u
    Just 0x67 -> parseStmtGUID u
    Just 0x70 -> parseStmtPath
    Just 0x6e -> parseStmtName u
    Just 0x6b -> parseStmtKill
    Just 0x73 -> parseStmtStat
    _         -> fail "bad statement"

parseStmts :: Using -> Parser [LQL]
parseStmts u = parseStmt u `sepBy1` newline

parseUsing :: User -> Parser Using
parseUsing user = do
  tree <- "using " .*> parseTree
  return (Using user tree)

parseLQL :: User -> Parser [LQL]
parseLQL u = parseUsing u `endBy` separator >>= parseLQL1

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
