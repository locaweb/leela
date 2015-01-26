{-# LANGUAGE TupleSections #-}

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

module Leela.Storage.Backend.ZMQ
    ( ZMQBackend ()
    , zmqbackend
    ) where

import qualified Data.Set as S
import           Data.Maybe
import           Leela.Logger
import           Control.Monad
import qualified Data.ByteString as B
import           Leela.Data.Time
import           Leela.Data.Types
import           Control.Exception
import           Leela.HZMQ.Dealer
import           Leela.Data.Excepts
import           Leela.MonadHelpers
import           Control.Applicative
import           Leela.Storage.Graph
import           Leela.Storage.KeyValue
import           Leela.Storage.Backend.ZMQ.Protocol

newtype ZMQBackend cache = ZMQBackend (Logger, Client, cache)

dealer :: ZMQBackend a -> Client
dealer (ZMQBackend (_, v, _)) = v

cachedb :: ZMQBackend a -> a
cachedb (ZMQBackend (_, _, v)) = v

syslog :: ZMQBackend a -> Logger
syslog (ZMQBackend (v, _, _)) = v

zmqbackend :: (KeyValue cache) => Logger -> Client -> cache -> ZMQBackend cache
zmqbackend a b c = ZMQBackend (a, b, c)

recv :: Maybe [B.ByteString] -> Reply
recv Nothing    = FailMsg 500
recv (Just msg) = decodeStrict msg

notFoundError :: String -> IO a
notFoundError m =
  throwIO (NotFoundExcept (Just m))

internalError :: String -> IO a
internalError m =
  throwIO (SystemExcept (Just m))

send :: Client -> Query -> IO Reply
send pool req = let msg = encodeLazy req
                in request pool msg >>= evaluate . recv

send_ :: Client -> Query -> IO ()
send_ pool req = do
  reply <- send pool req
  case reply of
    DoneMsg -> return ()
    _       -> internalError "ZMQ/send_: error sending request"

wrapCallback :: (Maybe [(GUID, Attr, Time, Value)] -> IO ()) -> Maybe [(B.ByteString, B.ByteString)] -> IO ()
wrapCallback callback Nothing       =
  callback Nothing
wrapCallback callback (Just values) =
  callback (Just $ mapMaybe decodeKeyVal values)
    where
      decodeKeyVal (key, val) = 
        let (g, a) = cacheUnkey key
        in (\(t, v) -> (g, a, t, v)) <$> cacheUnval val

instance (KeyValue a) => AttrBackend (ZMQBackend a) where

  putAttr _ []    = return ()
  putAttr m attrs = send_ (dealer m) (MsgPutAttr $ map setIndexing attrs)
      where
        setIndexing (g, a, v, o) = (g, a, v, setOpt Indexing o)

  putTAttr _ []    = return ()
  putTAttr m attrs = do
    seenset <- liftM (S.fromList . map fst . filter snd) $ mapM seen attrs
    send_ (dealer m) (MsgPutTAttr $ map (setIndexing seenset) attrs)
    void $ mapM storeCache attrs
      where
        setIndexing seenset (g, a, t, v, o)
          | S.member (g, a) seenset = (g, a, t, v, o)
          | otherwise               = (g, a, t, v, setOpt Indexing o)

        storeCache (g, a, t, v, _) =
          void (insertLazy (cachedb m) 86400 g (cacheKey g a) (cacheVal t v)
                 `catch` (warnAndReturn "storeCache# error writing to redis" False))

        seen (g, a, _, _, _) =
          liftM ((g, a), ) $ existsLazy (cachedb m) g (cacheKey g a)

        warnAndReturn :: String -> a -> SomeException -> IO a
        warnAndReturn msg value e = do
          warning (syslog m) (msg ++ " / " ++ show e)
          return value

  scanLast m (Just g) a callback =
    scanOnLazy (cachedb m) g (cacheKeyGlob (Just g) a) (wrapCallback callback)
  scanLast m Nothing a callback  =
    scanAllLazy (cachedb m) (cacheKeyGlob Nothing a) (wrapCallback callback)

  getAttr m a k   = do
    reply <- send (dealer m) (MsgGetAttr a k)
    case reply of
      KAttrMsg v  -> return (Just v)
      FailMsg 404 -> return Nothing
      _           -> internalError "ZMQ/getAttr: backend error"

  getTAttr m g a t l = do
    reply <- send (dealer m) (MsgGetTAttr g a t l)
    case reply of
      TAttrMsg v  -> return v
      FailMsg 404 -> notFoundError "ZMQ/getTAttr: not values found"
      _           -> internalError "ZMQ/getTAttr: backend error"

  listAttr m g page limit = do
    reply <- send (dealer m) (MsgListAttr g page limit)
    case reply of
      NAttrMsg xs -> return xs
      _           -> internalError "ZMQ/listAttr: backend error"

  listTAttr m g page limit = do
    reply <- send (dealer m) (MsgListTAttr g page limit)
    case reply of
      NAttrMsg xs -> return xs
      _           -> internalError "ZMQ/listTAttr: backend error"

  delAttr _ []    = return ()
  delAttr m attrs = send_ (dealer m) (MsgDelAttr attrs)

instance GraphBackend (ZMQBackend a) where

  getName m guids
    | length guids == 1 = mapM fetchGUID guids
    | otherwise         = mapMaybeM maybeFetchGUID guids
      where
        fetchGUID g = do
          reply <- send (dealer m) (MsgGetName g)
          case reply of
            NameMsg u t k n _ -> return (u, t, k, n, g)
            FailMsg 404       -> notFoundError "ZMQ/getName: name not found"
            _                 -> internalError "ZMQ/getName: backend error"

        maybeFetchGUID = liftM (either (const Nothing) Just)
                           . tryJust (guard . isNotFoundExcept)
                           . fetchGUID

  getGUID m names
    | length names == 1 = mapM fetchName names
    | otherwise         = mapMaybeM maybeFetchName names
       where
         fetchName (u, t, k, n) = do
           reply <- send (dealer m) (MsgGetGUID u t k n)
           case reply of
             NameMsg _ _ _ _ g -> return (u, t, k, n, g)
             FailMsg 404       -> notFoundError "ZMQ/getGUID: guid not found "
             _                 -> internalError "ZMQ/getGUID: backend error"

         maybeFetchName = liftM (either (const Nothing) Just)
                            . tryJust (guard . isNotFoundExcept)
                            . fetchName

  putName m u t k n = do
    reply <- send (dealer m) (MsgPutName u t k n)
    case reply of
      NameMsg _ _ _ _ g -> return g
      _                 -> internalError "ZMQ/putName: backend error"

  getLabel m g page limit = do
    reply <- send (dealer m) (MsgGetLabel g page limit)
    case reply of
      LabelMsg xs -> return xs
      _           -> internalError "ZMQ/getLabel: backend error"

  putLabel _ []     = return ()
  putLabel m labels = send_ (dealer m) (MsgPutLabel labels)

  hasLink m a l b = do
    reply <- send (dealer m) (MsgHasLink a l b)
    case reply of
      LinkMsg [] -> return False
      LinkMsg _  -> return True
      _          -> internalError "ZMQ/hasLink: backend error"

  getLink m a l page limit = do
    reply <- send (dealer m) (MsgGetLink a l page limit)
    case reply of
      LinkMsg xs -> return xs
      _          -> internalError "ZMQ/getLink: backend error"

  putLink _ []    = return ()
  putLink m links = send_ (dealer m) (MsgPutLink links)

  unlink _ []    = return ()
  unlink m links = send_ (dealer m) (MsgUnlink links)

  remove m a = send_ (dealer m) (MsgDelete a)
