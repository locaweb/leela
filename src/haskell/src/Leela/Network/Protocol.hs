{-# LANGUAGE OverloadedStrings #-}

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

module Leela.Network.Protocol
    ( FH
    , MAC (..)
    , User (..)
    , Nonce (..)
    , Query (..)
    , Reply (..)
    , RValue (..)
    , Writer
    , Signature (..)
    , isEOF
    , reduce
    , decode
    , encode
    , encodeE
    , makeList
    , namespaceFrom
    ) where

import           Data.Word
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Control.Exception
import           Leela.Data.Excepts
import           Control.Applicative
import           Data.ByteString.UTF8 (fromString)
import           Leela.Data.Namespace
import qualified Data.ByteString.Char8 as B8

type FH = Word64

type Writer a = a -> IO ()

newtype MAC = MAC B.ByteString

newtype User = User B.ByteString

newtype Nonce = Nonce B.ByteString

data Signature = Signature { sigUser  :: User
                           , sigTime  :: Time
                           , sigNonce :: Nonce
                           , sigMAC   :: MAC
                           }

data Query = Begin Signature [B.ByteString]
           | Close Bool Signature FH
           | Fetch Signature FH

data Reply = Done FH
           | Last (Maybe RValue)
           | Item RValue
           | Fail Int (Maybe String)

data RValue = Path [(GUID, Label)]
            | Stat [(B.ByteString, B.ByteString)]
            | List [RValue]
            | Name GUID B.ByteString B.ByteString Key

isEOF :: Reply -> Bool
isEOF m = isLast m || isFail m

isLast :: Reply -> Bool
isLast (Last _) = True
isLast _        = False

isFail :: Reply -> Bool
isFail (Fail _ _) = True
isFail _          = False

makeList :: [RValue] -> RValue
makeList [x] = x
makeList xs  = List xs

readDecimal :: (Integral n) => B.ByteString -> Either Reply n
readDecimal s = case (B8.readInteger s) of
                  Just (n, "") -> Right (fromIntegral n)
                  _            -> Left $ Fail 400 (Just "syntax error: invalid number")

readTime :: B.ByteString -> Either Reply Time
readTime = fmap (flip mktime 0) . readDecimal

readSignature :: B.ByteString -> Either Reply Signature
readSignature s =
  let (base, mac) = B.break (== 0x20) s
  in case (B.splitWith (== 0x3a) base) of
       [user, nonce, timestr] -> do
         t <- readTime timestr
         Right $ Signature (User user) t (Nonce nonce) (MAC $ B.drop 1 mac)
       _                      ->
         Left $ Fail 400 (Just "syntax error: signature")

encodeShow :: (Show s) => s -> B.ByteString
encodeShow = B8.pack . show

decode :: [B.ByteString] -> Either Reply Query
decode (sig:"begin":lql)         = fmap (flip Begin lql) (readSignature sig)
decode [sig,"close",fh]          = liftM2 (Close False) (readSignature sig) (readDecimal fh)
decode [sig,"close",fh,"nowait"] = liftM2 (Close True) (readSignature sig) (readDecimal fh)
decode [sig,"fetch",fh]          = liftM2 Fetch (readSignature sig) (readDecimal fh)
decode _                         = Left $ Fail 400 (Just "syntax error: bad frame")

encodeRValue :: RValue -> [B.ByteString]
encodeRValue (Name g u n k) = ["name", unpack g, u, n, unpack k]
encodeRValue (Path p)       = let f (g, l) acc = unpack l : unpack g : acc
                              in "path" : encodeShow (2 * length p) : foldr f [] p
encodeRValue (List v)       = "list" : encodeShow (length v) : concatMap encodeRValue v
encodeRValue (Stat prop)    = "stat" : encodeShow (2 * length prop) : concatMap (\(a, b) -> [a, b]) prop

encode :: Reply -> [B.ByteString]
encode (Done fh)              = ["done", encodeShow fh]
encode (Last Nothing)         = ["done"]
encode (Last (Just v))        = "done" : encodeRValue v
encode (Fail code Nothing)    = ["fail", encodeShow code]
encode (Fail code (Just msg)) = ["fail", encodeShow code, fromString msg]
encode (Item v)               = "item" : encodeRValue v

encodeE :: SomeException -> Reply
encodeE e = 
  case (fromException e) of
    Just BadDeviceExcept -> Fail 599 Nothing
    Just NotFoundExcept  -> Fail 404 Nothing
    Just TimeoutExcept   -> Fail 589 Nothing
    Just UserExcept      -> Fail 400 Nothing
    _                    -> Fail 500 Nothing

reduceRValue :: RValue -> RValue -> RValue
reduceRValue (List a) (List b) = List (a ++ b)
reduceRValue (List a) b        = List (b : a)
reduceRValue a (List b)        = List (a : b)
reduceRValue a b               = List [a, b]

reduce :: Reply -> Reply -> Reply
reduce m@(Fail _ _) _    = m
reduce _ m@(Fail _ _)    = m
reduce m@(Done _) _      = m
reduce _ m@(Done _)      = m
reduce (Last a) (Last b) = Last (liftA2 reduceRValue a b <|> a <|> b)
reduce (Last a) (Item b) = Last ((reduceRValue b <$> a) <|> (Just b))
reduce (Item a) (Last b) = Last ((reduceRValue a <$> b) <|> (Just a))
reduce (Item a) (Item b) = Item (reduceRValue a b)

namespaceFrom :: Signature -> Namespace
namespaceFrom = (\(User u) -> derive tld u) . sigUser
