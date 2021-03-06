
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
    , Tick
    , User (..)
    , Query (..)
    , Reply (..)
    , RValue (..)
    , Writer
    , Signature (..)
    , isEOF
    , decode
    , encode
    , encodeE
    , mergeReply
    , mapToLazyBS
    , encodeValue
    , mergeReplies
    , encodeDouble
    ) where

import           Data.List (foldl')
import           Data.Word
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Types
import           Control.Exception
import           Leela.DataHelpers
import           Leela.Data.Excepts
import           Leela.Data.Signature
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Builder
import           Data.Double.Conversion.ByteString

type FH = Word32

type Tick = Word32

type Writer a = a -> IO ()

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
            | Stat [(L.ByteString, L.ByteString)]
            | List [RValue]
            | KAttr GUID Attr (Maybe Value)
            | TAttr GUID Attr [(Time, Value)]
            | NAttrs GUID [Attr]
            | Name User Tree Kind Node GUID

isEOF :: Reply -> Bool
isEOF m = isLast m || isFail m

isLast :: Reply -> Bool
isLast Last = True
isLast _    = False

isFail :: Reply -> Bool
isFail (Fail _ _) = True
isFail _          = False

mergeReply :: Reply -> Reply -> Maybe Reply
mergeReply Last b              = Just $ b
mergeReply a Last              = Just $ a
mergeReply _ b@(Fail {})       = Just $ b
mergeReply a@(Fail {}) _       = Just $ a
mergeReply (Item x) (Item y)   = Just $ Item $ mergeRValue x y
mergeReply (Done _) _          = Nothing
mergeReply _ (Done _)          = Nothing

mergeRValue :: RValue -> RValue -> RValue
mergeRValue (List xs) (List ys) = List (xs ++ ys)
mergeRValue (List xs) y         = List (xs ++ [y])
mergeRValue x (List ys)         = List (x : ys)
mergeRValue x y                 = List [x, y]

mergeReplies :: [Reply] -> Maybe Reply
mergeReplies []           = Nothing
mergeReplies [x]          = Just x
mergeReplies (x : y : xs) = maybe Nothing (mergeReplies . (: xs)) (mergeReply x y)

readDecimal :: (Integral n) => B.ByteString -> Either Reply n
readDecimal s = case (B8.readInteger s) of
                  Just (n, "") -> Right (fromIntegral n)
                  _            -> Left $ Fail 400 (Just "syntax error: invalid number")

readTime :: B.ByteString -> Either Reply Time
readTime = fmap (fromSeconds . fromInt) . readDecimal
    where
      fromInt :: Int -> Double
      fromInt = fromIntegral

whenValidSignature :: (Time, NominalDiffTime) -> (User -> Maybe Secret) -> Time -> User -> Nonce -> MAC -> B.ByteString -> a -> Either Reply a
whenValidSignature now secLookup t u nonce sig msg =
  case (secLookup u) of
    Nothing  -> const $ Left $ Fail 401 $ Just "signature error: user"
    Just sec
      | not $ verify sec nonce msg sig -> const $ Left $ Fail 403 $ Just "signature error: secret"
      | expired now t                  -> const $ Left $ Fail 403 $ Just "signature error: expired"
      | otherwise                      -> Right

readNonce :: B.ByteString -> Either Reply Nonce
readNonce = maybe (Left $ Fail 400 $ Just "signature error: nonce") Right . initNonce . fst . B16.decode

readMAC :: B.ByteString -> Either Reply MAC
readMAC = maybe (Left $ Fail 400 (Just "signature error: mac")) Right . initMAC . fst . B16.decode

readSignature :: (Time, NominalDiffTime) -> (User -> Maybe Secret) -> B.ByteString -> [B.ByteString] -> Either Reply Signature
readSignature now users sig msg =
  let (base, mac) = B.break (== 0x20) sig
  in case (B.splitWith (== 0x3a) base) of
       [user, timestr, nonce] -> do
         let u = userFromBS user
         t0 <- readTime timestr
         n  <- readNonce nonce
         s  <- readMAC $ B.drop 1 mac
         whenValidSignature now users t0 u n s (B.concat (base : B.singleton 0x3a : msg)) (Signature u t0 n s)
       _                      ->
         Left $ Fail 400 (Just "syntax error: signature")

decode :: (Time, NominalDiffTime) -> (User -> Maybe Secret) -> [B.ByteString] -> Either Reply Query
decode now users (sig:"begin":lql)         = fmap (flip Begin lql) (readSignature now users sig ("begin" : lql))
decode now users [sig,"close",fh]          = liftM2 (Close False) (readSignature now users sig ["close",fh]) (readDecimal fh)
decode now users [sig,"close",fh,"nowait"] = liftM2 (Close True) (readSignature now users sig ["close",fh,"nowait"]) (readDecimal fh)
decode now users [sig,"fetch",fh]          = liftM2 Fetch (readSignature now users sig ["fetch",fh]) (readDecimal fh)
decode _ _ _                               = Left $ Fail 400 (Just "syntax error: bad frame")

encodeDouble :: Double -> L.ByteString
encodeDouble = L.fromStrict . toShortest

encodeValue :: Value -> [L.ByteString]
encodeValue (Bool True)  = ["0", "true"]
encodeValue (Bool False) = ["0", "false"]
encodeValue (Text v)     = ["1", v]
encodeValue (Int32 v)    = ["2", toLazyBS 32 $ int32Dec v]
encodeValue (Int64 v)    = ["3", toLazyBS 32 $ int64Dec v]
encodeValue (UInt32 v)   = ["4", toLazyBS 32 $ word32Dec v]
encodeValue (UInt64 v)   = ["5", toLazyBS 32 $ word64Dec v]
encodeValue (Double v)   = ["6", encodeDouble v]

encodeTimeSeries :: [(Time, Value)] -> [L.ByteString]
encodeTimeSeries = concatMap (\(t, v) -> (encodeDouble (seconds t)) : encodeValue v)

encodeRValue :: RValue -> [L.ByteString]
encodeRValue (Name u t k n g)     = [ "name"
                                    , asLazyByteString u
                                    , asLazyByteString t
                                    , asLazyByteString k
                                    , asLazyByteString n
                                    , asLazyByteString g
                                    ]
encodeRValue (Path p)             =   "path"
                                    : toLazyBS 32 (intDec (length p))
                                    : (foldl' (\acc (g, l) -> asLazyByteString l : asLazyByteString g : acc) [] p)
encodeRValue (List v)             =   "list"
                                    : toLazyBS 32 (intDec (length v))
                                    : concatMap encodeRValue v
encodeRValue (Stat prop)          =   "stat"
                                    : toLazyBS 32 (intDec (length prop))
                                    : concatMap (\(a, b) -> [a, b]) prop
encodeRValue (TAttr g a v)        =   "t-attr"
                                    : toLazyBS 32 (intDec (length v))
                                    : asLazyByteString g
                                    : asLazyByteString a
                                    : encodeTimeSeries v
encodeRValue (KAttr g a Nothing)  = [ "k-attr"
                                    , asLazyByteString g
                                    , asLazyByteString a
                                    , "-1"
                                    , L.empty
                                    ]
encodeRValue (KAttr g a (Just v)) =   "k-attr"
                                    : asLazyByteString g
                                    : asLazyByteString a
                                    : encodeValue v
encodeRValue (NAttrs g names)     =   "n-attr"
                                    : toLazyBS 32 (intDec (length names))
                                    : asLazyByteString g
                                    : (map asLazyByteString names)

encode :: Reply -> [L.ByteString]
encode (Done fh)              = [ "done"
                                , toLazyBS 32 $ word32Dec fh
                                ]
encode Last                   = [ "done" ]
encode (Fail code Nothing)    = [ "fail"
                                , toLazyBS 32 $ intDec code
                                , defaultMessage code
                                ]
encode (Fail code (Just msg)) = [ "fail"
                                , toLazyBS 32 $ intDec code
                                , toLazyBS 512 $ string7 msg
                                ]
encode (Item v)               =   "item"
                                : encodeRValue v

defaultMessage :: Int -> L.ByteString
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
    Just (BadDeviceExcept m) -> Fail 599 m
    Just (NotFoundExcept m)  -> Fail 404 m
    Just (TimeoutExcept m)   -> Fail 589 m
    Just (UserExcept m)      -> Fail 400 m
    _                        -> Fail 500 Nothing
