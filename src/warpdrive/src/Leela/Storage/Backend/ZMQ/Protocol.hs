{-# LANGUAGE TupleSections     #-}
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

module Leela.Storage.Backend.ZMQ.Protocol
    ( Query (..)
    , Reply (..)
    , cacheKey
    , cacheVal
    , encodeLazy
    , cacheUnkey
    , cacheUnval
    , mapToLazyBS
    , cacheKeyGlob
    , decodeStrict
    ) where

import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Serialize as E
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Data.MessagePack
import           Leela.Data.Types
import           Leela.DataHelpers
import           Data.Text.Encoding (encodeUtf8)
import           Control.Applicative
import           Leela.Storage.Graph (Limit)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder

data Query = MsgGetName GUID
           | MsgGetGUID User Tree Kind Node
           | MsgPutName User Tree Kind Node
           | MsgPutLink [(GUID, Label, GUID)]
           | MsgPutLabel [(GUID, Label)]
           | MsgGetLink GUID Label (Maybe GUID) Limit
           | MsgHasLink GUID Label GUID
           | MsgGetLabel GUID (Mode Label) Limit
           | MsgUnlink [(GUID, Label, Maybe GUID)]
           | MsgPutAttr [(GUID, Attr, Value, [Option])]
           | MsgPutTAttr [(GUID, Attr, Time, Value, [Option])]
           | MsgDelAttr [(GUID, Attr)]
           | MsgGetAttr GUID Attr
           | MsgGetTAttr GUID Attr Time Limit
           | MsgListAttr GUID (Mode Attr) Limit
           | MsgListTAttr GUID (Mode Attr) Limit
           | MsgDelete GUID

data Reply = DoneMsg
           | NameMsg User Tree Kind Node GUID
           | LinkMsg [GUID]
           | KAttrMsg Value
           | TAttrMsg [(Time, Value)]
           | NAttrMsg [Attr]
           | LabelMsg [Label]
           | FailMsg Int

encodeLazyByteString :: L.ByteString -> Object
encodeLazyByteString = ObjectBinary . L.toStrict

decodeByteString :: Object -> Maybe B.ByteString
decodeByteString (ObjectBinary b) = Just b
decodeByteString (ObjectString b) = Just $ encodeUtf8 b
decodeByteString _                = Nothing

asObjBinary :: AsLazyByteString x => x -> Object
asObjBinary = encodeLazyByteString . asLazyByteString

objectInt :: Int -> Object
objectInt = ObjectInt . fromIntegral

msgPackMagicL :: L.ByteString
msgPackMagicL = "000"

msgPackMagicS :: B.ByteString
msgPackMagicS = "000"

truncateInt :: Double -> Int
truncateInt = truncate

cacheKeyGlob :: Maybe GUID -> Attr -> L.ByteString
cacheKeyGlob Nothing (Attr "*")       = "*"
cacheKeyGlob Nothing (Attr a)         = 0x2a `L.cons` 0 `L.cons` a
cacheKeyGlob (Just (GUID g)) (Attr a) = g `L.append` (0 `L.cons` a)

cacheKey :: GUID -> Attr -> L.ByteString
cacheKey (GUID g) (Attr a) = toLazyBS 128 (lazyByteString g <> char7 '\0' <> lazyByteString a)

cacheUnkey :: B.ByteString -> (GUID, Attr)
cacheUnkey raw = let (guid, attr) = B.break (== 0) raw
                 in (guidFromBS guid, attrFromBS $ B.drop 1 attr)

cacheVal :: Time -> Value -> B.ByteString
cacheVal time value = E.encode (time, value)

cacheUnval :: B.ByteString -> Maybe (Time, Value)
cacheUnval = either (const Nothing) Just . E.decode

decodeValue :: Object -> Maybe (Time, Value)
decodeValue obj = case obj of
                     ObjectArray [ObjectInt t, objVal]
                       -> (fromSeconds $ fromIntegral t ,) <$> (decodeMaybe =<< decodeByteString objVal)
                     _ -> Nothing
    where
      decodeMaybe = either (const Nothing) Just . E.decode

encodeMode :: (AsLazyByteString s) => GUID -> Mode s -> [Object]
encodeMode g (All Nothing)  = [ ObjectString "all"
                              , asObjBinary g
                              , encodeLazyByteString L.empty
                              ]
encodeMode g (All (Just l)) = [ ObjectString "all"
                              , asObjBinary g
                              , asObjBinary l
                              ]
encodeMode g (Prefix a b)   = [ ObjectString "pre"
                              , asObjBinary g
                              , asObjBinary a
                              , asObjBinary b
                              ]
encodeMode g (Precise l)    = [ ObjectString "ext"
                              , asObjBinary g
                              , asObjBinary l
                              ]

encodeOptions :: [Option] -> M.Map Object Object
encodeOptions = M.fromList . mapMaybe encodeOption
    where
      encodeOption (TTL v)    = Just (ObjectString "ttl", objectInt v)
      encodeOption (Indexing) = Just (ObjectString "index", ObjectBool True)
      encodeOption _          = Nothing

msgGetName :: GUID -> Object
msgGetName g = ObjectArray [ ObjectString "get"
                           , ObjectString "name"
                           , ObjectArray [ asObjBinary g ]
                           ]

msgHasLink :: GUID -> Label -> GUID -> Object
msgHasLink a l b = ObjectArray [ ObjectString "get"
                               , ObjectString "link"
                               , ObjectArray [ asObjBinary a
                                             , asObjBinary l
                                             , asObjBinary b
                                             ]
                               , objectInt 1
                               ]

msgGetGUID :: User -> Tree -> Kind -> Node -> Object
msgGetGUID u t k n = ObjectArray [ ObjectString "get"
                                 , ObjectString "guid"
                                 , ObjectArray [ asObjBinary u
                                               , asObjBinary t
                                               , asObjBinary k
                                               , asObjBinary n
                                               ]
                                 ]

msgGetLink :: GUID -> Label -> Maybe GUID -> Limit -> Object
msgGetLink g l page lim = ObjectArray [ ObjectString "get"
                                      , ObjectString "link"
                                      , ObjectArray [ asObjBinary g
                                                    , asObjBinary l
                                                    , maybe (ObjectString "") asObjBinary page
                                                    ]
                                      , objectInt lim
                                      ]

msgGetLabel :: GUID -> Mode Label -> Limit -> Object
msgGetLabel g mode lim = ObjectArray [ ObjectString "get"
                                     , ObjectString "label"
                                     , ObjectArray $ encodeMode g mode
                                     , objectInt lim
                                     ]

msgPutName :: User -> Tree -> Kind -> Node -> Object
msgPutName u t k n = ObjectArray [ ObjectString "put"
                                 , ObjectString "name"
                                 , ObjectArray [ asObjBinary u
                                               , asObjBinary t
                                               , asObjBinary k
                                               , asObjBinary n
                                               ]
                                 ]

msgPutLink :: [(GUID, Label, GUID)] -> Object
msgPutLink links = ObjectArray [ ObjectString "put"
                               , ObjectString "link"
                               , ObjectArray $ map mkTuple links
                               ]
    where
      mkTuple (a, l, b) = ObjectArray [ asObjBinary a, asObjBinary l, asObjBinary b ]

msgPutLabel :: [(GUID, Label)] -> Object
msgPutLabel labels = ObjectArray [ ObjectString "put"
                                 , ObjectString "label"
                                 , ObjectArray $ map mkTuple labels
                                 ]
    where
      mkTuple (a, l) = ObjectArray [ asObjBinary a, asObjBinary l ]

msgUnlink :: [(GUID, Label, Maybe GUID)] -> Object
msgUnlink links = ObjectArray [ ObjectString "del"
                              , ObjectString "link"
                              , ObjectArray $ map mkTuple links
                              ]
    where
      mkTuple (a, l, mb)  = ObjectArray [ asObjBinary a
                                        , asObjBinary l
                                        , maybe (ObjectBinary "") asObjBinary mb
                                        ]

msgPutKAttr :: [(GUID, Attr, Value, [Option])] -> Object
msgPutKAttr attrs = ObjectArray [ ObjectString "put"
                                , ObjectString "k-attr"
                                , ObjectArray $ map mkTuple attrs
                                ]
    where
      mkTuple (g, a, v, o) = ObjectArray [ asObjBinary g
                                         , asObjBinary a
                                         , ObjectBinary $ E.encode v
                                         , ObjectMap $ encodeOptions o
                                         ]

msgPutTAttr :: [(GUID, Attr, Time, Value, [Option])] -> Object
msgPutTAttr attrs = ObjectArray [ ObjectString "put"
                                , ObjectString "t-attr"
                                , ObjectArray $ map mkTuple attrs
                                ]
    where
      mkTuple (g, a, t, v, o) = ObjectArray [ asObjBinary g
                                            , asObjBinary a
                                            , objectInt (truncateInt $ seconds t)
                                            , ObjectBinary $ E.encode v
                                            , ObjectMap $ encodeOptions o
                                            ]

msgDelKAttr :: [(GUID, Attr)] -> Object
msgDelKAttr attrs = ObjectArray [ ObjectString "del"
                                , ObjectString "k-attr"
                                , ObjectArray $ map mkTuple attrs
                                ]
    where
      mkTuple (g, a) = ObjectArray [ asObjBinary g, asObjBinary a ]

msgGetKAttr :: GUID -> Attr -> Object
msgGetKAttr g a = ObjectArray [ ObjectString "get"
                              , ObjectString "k-attr"
                              , ObjectArray [ asObjBinary g
                                            , asObjBinary a
                                            ]
                              ]

msgGetTAttr :: GUID -> Attr -> Time -> Limit -> Object
msgGetTAttr g a t l = ObjectArray [ ObjectString "get"
                                  , ObjectString "t-attr"
                                  , ObjectArray [ asObjBinary g
                                                , asObjBinary a
                                                , objectInt (truncateInt $ seconds t)
                                                ]
                                  , objectInt l
                                  ]

msgListKAttr :: GUID -> Mode Attr -> Limit -> Object
msgListKAttr g m l = ObjectArray [ ObjectString "get"
                                 , ObjectString "attr"
                                 , ObjectString "k-attr"
                                 , ObjectArray $ encodeMode g m
                                 , objectInt l
                                 ]

msgListTAttr :: GUID -> Mode Attr -> Limit -> Object
msgListTAttr g m l = ObjectArray [ ObjectString "get"
                                 , ObjectString "attr"
                                 , ObjectString "t-attr"
                                 , ObjectArray $ encodeMode g m
                                 , objectInt l
                                 ]

msgDelete :: GUID -> Object
msgDelete g = ObjectArray [ ObjectString "del"
                          , ObjectString "node"
                          , ObjectArray [asObjBinary g]
                          ]

encode :: Query -> Object
encode (MsgGetName g)                = msgGetName g
encode (MsgGetGUID u t k n)          = msgGetGUID u t k n
encode (MsgHasLink a l b)            = msgHasLink a l b
encode (MsgGetLink g l page lim)     = msgGetLink g l page lim
encode (MsgGetLabel g m lim)         = msgGetLabel g m lim
encode (MsgPutName u t k n)          = msgPutName u t k n
encode (MsgPutLink links)            = msgPutLink links
encode (MsgPutLabel labels)          = msgPutLabel labels
encode (MsgUnlink links)             = msgUnlink links
encode (MsgPutAttr attrs)            = msgPutKAttr attrs
encode (MsgPutTAttr attrs)           = msgPutTAttr attrs
encode (MsgDelAttr attrs)            = msgDelKAttr attrs
encode (MsgGetAttr g a)              = msgGetKAttr g a
encode (MsgGetTAttr g a t l)         = msgGetTAttr g a t l
encode (MsgListAttr g mode lim)      = msgListKAttr g mode lim
encode (MsgListTAttr g mode lim)     = msgListTAttr g mode lim
encode (MsgDelete a)                 = msgDelete a

encodeLazy :: Query -> [L.ByteString]
encodeLazy = (msgPackMagicL :) . (: []) . E.encodeLazy . encode

decodeNameMsg :: Object -> Maybe Reply
decodeNameMsg (ObjectArray [u, t, k, n, g]) = NameMsg <$> (userFromBS <$> decodeByteString u)
                                                      <*> (treeFromBS <$> decodeByteString t)
                                                      <*> (kindFromBS <$> decodeByteString k)
                                                      <*> (nodeFromBS <$> decodeByteString n)
                                                      <*> (guidFromBS <$> decodeByteString g)
decodeNameMsg _                             = Nothing

decodeLinkMsg :: Object -> Maybe Reply
decodeLinkMsg (ObjectArray guids) = LinkMsg <$> sequence (map (liftA guidFromBS . decodeByteString) guids)
decodeLinkMsg _                   = Nothing

decodeLabelMsg :: Object -> Maybe Reply
decodeLabelMsg (ObjectArray labels) = LabelMsg <$> sequence (map (liftA labelFromBS . decodeByteString) labels)
decodeLabelMsg _                    = Nothing

decodeNAttrMsg :: Object -> Maybe Reply
decodeNAttrMsg (ObjectArray names) = NAttrMsg <$> sequence (map (liftA attrFromBS . decodeByteString) names)
decodeNAttrMsg _                   = Nothing

decodeKAttrMsg :: Object -> Maybe Reply
decodeKAttrMsg value = either (const $ FailMsg 599) KAttrMsg <$> E.decode <$> decodeByteString value

decodeTAttrMsg :: Object -> Maybe Reply
decodeTAttrMsg (ObjectArray values) = TAttrMsg <$> sequence (map decodeValue values)
decodeTAttrMsg _                    = Nothing

decodeFailMsg :: Object -> Maybe Reply
decodeFailMsg (ObjectInt code) = Just $ FailMsg (fromIntegral code)
decodeFailMsg _                = Nothing

decodeStrict :: [B.ByteString] -> Reply
decodeStrict [magic, obj]
  | magic == msgPackMagicS = case (E.decode obj) of
                               Right (ObjectArray [code, args])
                                 -> case (decodeByteString code) of
                                      Just "done"   -> DoneMsg
                                      Just "fail"   -> maybe (FailMsg 599) id $ decodeFailMsg args
                                      Just "link"   -> maybe (FailMsg 599) id $ decodeLinkMsg args
                                      Just "name"   -> maybe (FailMsg 599) id $ decodeNameMsg args
                                      Just "label"  -> maybe (FailMsg 599) id $ decodeLabelMsg args
                                      Just "k-attr" -> maybe (FailMsg 599) id $ decodeKAttrMsg args
                                      Just "n-attr" -> maybe (FailMsg 599) id $ decodeNAttrMsg args
                                      Just "t-attr" -> maybe (FailMsg 599) id $ decodeTAttrMsg args
                                      _             -> FailMsg 599
                               _ -> FailMsg 599
  | otherwise             = FailMsg 599
decodeStrict _            = FailMsg 599
