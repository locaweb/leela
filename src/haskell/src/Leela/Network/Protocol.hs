{-# LANGUAGE OverloadedStrings #-}

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
    ) where

import           Data.List (foldl')
import           Data.Word
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Control.Exception
import           Leela.Data.Types
import           Leela.Data.Excepts
import           Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Char8 as B8
import           Data.Double.Conversion.ByteString

type FH = Word64

type Writer a = a -> IO ()

newtype MAC = MAC B.ByteString

newtype Nonce = Nonce B.ByteString

data Signature = Signature { sigUser  :: User
                           , sigTime  :: Time
                           , sigNonce :: Nonce
                           , sigMAC   :: MAC
                           }

data Query = Begin Signature [B.ByteString]
           | Close Bool Signature FH
           | Fetch Signature FH

data Reply = Last
           | Done FH
           | Item RValue
           | Fail Int (Maybe String)

data RValue = Path [(GUID, Label)]
            | Stat [(B.ByteString, B.ByteString)]
            | List [RValue]
            | KAttr GUID Attr (Maybe Value)
            | TAttr GUID Attr [(Time, Value)]
            | NAttrs GUID [Attr]
            | Name User Tree Node GUID

isEOF :: Reply -> Bool
isEOF m = isLast m || isFail m

isLast :: Reply -> Bool
isLast Last = True
isLast _    = False

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
readTime = fmap (fromSeconds . fromInt) . readDecimal
    where
      fromInt :: Int -> Double
      fromInt = fromIntegral

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

encodeValue :: Value -> [B.ByteString]
encodeValue (Bool True)  = ["0", "true"]
encodeValue (Bool False) = ["0", "false"]
encodeValue (Text v)     = ["1", v]
encodeValue (Int32 v)    = ["2", encodeShow v]
encodeValue (Int64 v)    = ["3", encodeShow v]
encodeValue (UInt32 v)   = ["4", encodeShow v]
encodeValue (UInt64 v)   = ["5", encodeShow v]
encodeValue (Double v)   = ["6", toShortest v]

encodeTimeSeries :: [(Time, Value)] -> [B.ByteString]
encodeTimeSeries = concatMap (\(t, v) -> toShortest (seconds t) : encodeValue v)

encodeRValue :: RValue -> [B.ByteString]
encodeRValue (Name u t n g)       = ["name", toByteString u, toByteString t, toByteString n, toByteString g]
encodeRValue (Path p)             = let f acc (g, l) = toByteString l : toByteString g : acc
                                    in "path" : encodeShow (length p) : foldl' f [] p
encodeRValue (List v)             = "list" : encodeShow (length v) : concatMap encodeRValue v
encodeRValue (Stat prop)          = "stat" : encodeShow (length prop) : concatMap (\(a, b) -> [a, b]) prop
encodeRValue (TAttr g a v)        = "t-attr" : (encodeShow $ length v) : toByteString g : toByteString a : encodeTimeSeries v
encodeRValue (KAttr g a Nothing)  = ["k-attr", toByteString g, toByteString a, "-1", ""]
encodeRValue (KAttr g a (Just v)) = "k-attr" : toByteString g : toByteString a : encodeValue v
encodeRValue (NAttrs g names)     = "n-attr" : encodeShow (length names) : toByteString g : map toByteString names

encode :: Reply -> [B.ByteString]
encode (Done fh)              = ["done", encodeShow fh]
encode Last                   = ["done"]
encode (Fail code Nothing)    = ["fail", encodeShow code, defaultMessage code]
encode (Fail code (Just msg)) = ["fail", encodeShow code, fromString msg]
encode (Item v)               = "item" : encodeRValue v

defaultMessage :: Int -> B.ByteString
defaultMessage 404  = "not found"
defaultMessage 403  = "forbidden"
defaultMessage 400  = "bad request"
defaultMessage code
  | code >= 400 && code < 500 = "user error"
  | code >= 500 && code < 600 = "internal server error"
  | otherwise                 = "error"

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
reduce m Last            = m
reduce Last m            = m
reduce (Item a) (Item b) = Item (reduceRValue a b)
