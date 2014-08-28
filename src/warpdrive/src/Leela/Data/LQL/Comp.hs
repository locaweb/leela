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
import           Data.Monoid (mconcat)
import           Control.Monad
import qualified Data.Sequence as S
import           Leela.Data.LQL
import           Leela.Data.Time
import qualified Data.ByteString as B
import           Leela.Data.Types
import           Control.Applicative
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as L
import           Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 (decimal, signed, double)

data Direction a = L a
                 | R a
                 | B a

class Source a where

    bytestring :: a -> B.ByteString

isSpace :: Parser Bool
isSpace = liftM (== Just 0x20) peekWord8

hardspace :: Parser ()
hardspace = void $ word8 0x20

parseNode :: Parser (Kind, Node)
parseNode = do
  (Kind kind) <- Kind <$> qstring 64 0x28 0x3a
  (Node name) <- Node <$> qstring 512 0x3a 0x29
  when (L.null kind) (fail "kind must not be null")
  when (L.null name) (fail "name must not be null")
  return (Kind kind, Node name)

parseTree :: Parser (Maybe User, Tree)
parseTree = do
  name <- qstring 512 0x28 0x29
  buildResult (L.break (== 0x3a) name)
    where
      buildResult (left, right)
        | L.null right        = return (Nothing, Tree left)
        | otherwise           = case (L.take 2 right) of
                                  "::" -> liftM2 (,) (Just <$> User <$> (validate left)) (Tree <$> (validate $ L.drop 2 right))
                                  _    -> fail "parseTree: syntax error"

      validate s
        | 0x3a `L.elem` s = fail "parseTree: syntax error"
        | otherwise       = return s

parseLabel :: Parser Label
parseLabel = Label <$> qstring 512 0x5b 0x5d

parseAttr :: Parser Attr
parseAttr = Attr <$> qstring 512 0x22 0x22

parseGUID :: Parser GUID
parseGUID = guidFromBS <$> (A.take 36)

parseGUIDAttr :: Parser (GUID, Attr)
parseGUIDAttr = do
  g <- parseGUID
  hardspace
  k <- parseAttr
  return (g, k)

parseMaybeGUID :: Parser (Maybe GUID)
parseMaybeGUID = ("()" *> return Nothing) <|> (Just <$> parseGUID)

qstring :: Int -> Word8 -> Word8 -> Parser L.ByteString
qstring limit l r = word8 l >> anyWord8 >>= loop (limit - 1) []
    where
      loop lim acc x
        | x == r    = return (L.pack $ reverse acc)
        | lim < 0   = fail "name is too long"
        | x == 0x5c = do
            c <- anyWord8
            anyWord8 >>= loop (lim - 1) (c : acc)
        | otherwise = anyWord8 >>= loop (lim - 1) (x : acc)

newline :: Parser ()
newline = void ((word8 0x2c >> word8 0x20) <|> word8 0x0a)

separator :: Parser ()
separator = void (word8 0x0a <|> word8 0x20)

semicolon :: Parser ()
semicolon = void $ word8 0x3b

parseLink :: Parser (Direction Label)
parseLink = do
  mdir <- word8 0x2d <|> word8 0x3c
  l    <- option (Label L.empty) parseLabel
  f    <- case mdir of
           0x2d -> (const B <$> (word8 0x2d)) <|> (const R <$> (word8 0x3e))
           0x3c -> (const L <$> (word8 0x2d))
           _    -> fail "parseLink"
  return (f l)

parseRLink :: Parser Label
parseRLink = do
  dlink <- parseLink
  case dlink of
    R l -> return l
    _   -> fail "parseRLink: wrong direction"

parseMakeCreate :: Parser (S.Seq Journal)
parseMakeCreate = do
  k <- parseGUID
  peekWord8 >>= go k []
    where
      go a g (Just 0x20) = do
        l <- hardspace >> parseLink
        b <- hardspace >> parseGUID
        peekWord8 >>= go b (asLink g a b l)
      go _ g _           = return (mconcat $ map (\(a, l, b) -> (S.fromList [PutLabel a l, PutLink a l b])) g)

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

nan :: Double
nan = 0/0

inf :: Double
inf = 1/0

parseDouble :: Parser Double
parseDouble =
  "nan" *> return nan
  <|> "inf" *> return inf
  <|> "-inf" *> return (negate inf)
  <|> signed double

parseValue :: Parser Value
parseValue = do
  mw <- peekWord8
  case mw of
    Just 0x22 -> Text <$> qstring (64 * 1024) 0x22 0x22
    Just 0x28 -> Int32 <$> ("(int32 " *> (signed decimal `endBy` word8 0x29))
                 <|> Int64 <$> ("(int64 " *> (signed decimal `endBy` word8 0x29))
                 <|> UInt32 <$> ("(uint32 " *> (decimal `endBy` word8 0x29))
                 <|> UInt64 <$> ("(uint64 " *> (decimal `endBy` word8 0x29))
                 <|> Double <$> ("(double " *> (parseDouble `endBy` word8 0x29))
                 <|> "(bool true)" *> return (Bool True)
                 <|> "(bool false)" *> return (Bool False)
    _         -> Double <$> parseDouble

parseTimePoint :: Parser Time
parseTimePoint = do
  _ <- word8 0x5b
  r <- fromSeconds <$> double
  _ <- word8 0x5d
  return r

parseTimeRange :: Parser TimeRange
parseTimeRange = do
  _  <- word8 0x5b
  t0 <- fromSeconds <$> double
  _  <- word8 0x3a
  t1 <- fromSeconds <$> double
  _  <- word8 0x5d
  return (Range t0 t1)

parseWithStmt :: Parser [Option]
parseWithStmt = "with " *> (parseOption `sepBy` (string ", "))
    where
      parseOption = "ttl:" *> (TTL <$> decimal)
                    <|> "max_data_points:" *> (MaxDataPoints <$> decimal)
                    <|> "data" *> (Data <$> qstring 64 0x3a 0x3d <*> qstring 512 0x22 0x22)

parseStmtMake :: Using -> Parser LQL
parseStmtMake u = do
  _  <- string "make "
  at <- peekWord8
  case at of
    Just 0x28 -> AlterStmt <$> S.singleton <$> uncurry (PutNode (targetUser u) (uTree u)) <$> parseNode
    Just _    -> AlterStmt <$> parseMakeCreate
    _         -> fail "bad make statement"

parseStmtPath :: Parser LQL
parseStmtPath = "path " *> (PathStmt <$> parseQuery)

parseStmtName :: Using -> Parser LQL
parseStmtName u = "name " *> (NameStmt u <$> (S.singleton <$> parseGUID))

parseStmtGUID :: Using -> Parser LQL
parseStmtGUID u = "guid " *> (GUIDStmt u <$> (S.singleton <$> parseNode))

parseStmtStat :: Parser LQL
parseStmtStat = "stat" *> return StatStmt

parseStmtKill :: Parser LQL
parseStmtKill = "kill " *> doParse
    where
      doParse = do
        ma <- parseMaybeGUID
        hardspace
        dl  <- parseLink
        hardspace
        mb <- parseMaybeGUID
        case (ma, mb, dl) of
          (Just a, Nothing, R l) -> return $ AlterStmt (S.singleton $ DelLink a l Nothing)
          (Just a, Just b, R l)  -> return $ AlterStmt (S.singleton $ DelLink a l (Just b))
          (Nothing, Just b, L l) -> return $ AlterStmt (S.singleton $ DelLink b l Nothing)
          (Just a, Just b, L l)  -> return $ AlterStmt (S.singleton $ DelLink b l (Just a))
          (Just a, Just b, B l)  -> return $ AlterStmt (S.fromList [DelLink a l (Just b), DelLink b l (Just a)])
          _                      -> fail "invalid kill command"

parseStmtAttr :: Parser LQL
parseStmtAttr = "attr put " *> parsePutAttr
                <|> "attr last * " *> parseLastAttrAll
                <|> "attr last " *> parseLastAttrOn
                <|> "attr get " *> parseGetAttr
                <|> "attr del " *> parseDelAttr
                <|> "attr kls " *> parseListAttr KAttrListStmt
                <|> "attr tls " *> parseListAttr TAttrListStmt
    where
      parsePutAttr = do
        (g, k) <- parseGUIDAttr
        hardspace
        token <- peekWord8
        case token of
          Just 0x5b -> parsePutTAttr g k
          _         -> parsePutKAttr g k

      parseLastAttrAll = do
        a <- parseAttr
        return (TAttrLastStmt Nothing a)

      parseLastAttrOn = do
        g <- parseGUID
        hardspace
        a <- parseAttr
        return (TAttrLastStmt (Just g) a)

      parsePutKAttr g k = do
        v <- parseValue
        w <- option [] (hardspace >> parseWithStmt)
        return (AlterStmt (S.singleton $ PutKAttr g k v w))

      parsePutTAttr g k = do
        t <- parseTimePoint
        hardspace
        v <- parseValue
        w <- option [] (hardspace >> parseWithStmt)
        return (AlterStmt (S.singleton $ PutTAttr g k t v w))

      parseGetAttr = do
        (g, a) <- parseGUIDAttr
        (TAttrGetStmt g a <$> (hardspace >> parseTimeRange) <*> (option [] (hardspace >> parseWithStmt)))
          <|> (KAttrGetStmt g a <$> (option [] (hardspace >> parseWithStmt)))

      parseListAttr kind = do
        (g, Attr a) <- parseGUIDAttr
        return (kind g (Attr <$> glob a))

      parseDelAttr = do
        (g, k) <- parseGUIDAttr
        AlterStmt <$> (S.singleton <$> ((DelTAttr g k <$> (hardspace >> parseTimeRange))
                                          <|> (return (DelKAttr g k))))

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
    Just 0x61 -> parseStmtAttr
    _         -> fail "bad statement"

parseStmts :: Using -> Parser [LQL]
parseStmts u = groupLQL <$> parseStmt u `sepBy1` newline

parseUsing :: User -> Parser Using
parseUsing user = do
  (asUser, tree) <- "using " *> parseTree
  return (Using user tree asUser)

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
