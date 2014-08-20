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

module Leela.Naming
       ( resolver
       ) where

import qualified Data.Map as M
import           Data.Word
import           Data.Aeson
import           Data.IORef
import           Leela.Logger
import           Network.HTTP
import           Control.Monad
import           Control.Exception
import           Leela.Data.Excepts
import           Control.Applicative
import           Leela.Data.Endpoint
import qualified Data.ByteString.Lazy.Char8 as L

data Service = Service { address     :: String
                       , serviceTags :: [String]
                       , servicePort :: Word16
                       , status      :: [(String, String)]
                       }
             deriving (Eq, Show)

isPassing :: Service -> Bool
isPassing = (== Just "passing") . lookup "leela" . status

parseService :: Service -> [(String, [Endpoint])]
parseService srv
  | isPassing srv = map (\tag -> (tag, [TCP (address srv) (servicePort srv) ""])) (serviceTags srv)
  | otherwise     = []

parseServices :: L.ByteString -> M.Map String [Endpoint]
parseServices = M.fromListWith (++) . concatMap parseService . maybe [] id . decode

fetchCatalog :: String -> String -> IO L.ByteString
fetchCatalog url key = do
  rsp          <- simpleHTTP (getRequest (url ++ key))
  (code, body) <- liftM2 (,) (getResponseCode rsp) (getResponseBody rsp)
  if (code == (2,0,0))
  then
    return $ L.pack body
  else
    throwIO (SystemExcept (Just $ "Naming/fetchCatalog: error fetching catalog: " ++ (show code)))

resolver :: Logger -> IORef [(String, [Endpoint])] -> String -> IO ()
resolver syslog ioref url = do
  info syslog (printf "resolver: fetching from consul: %s" url)
  oldServices <- readIORef ioref
  newServices <- fmap (M.toAscList . parseServices) (fetchCatalog url "/v1/health/service/leela")
  when (oldServices /= newServices && (not $ null newServices)) $ do
    atomicWriteIORef ioref newServices
    notice syslog (printf "resolver: %s" (show newServices))

instance FromJSON Service where
   parseJSON (Object v) = Service <$>
                            ((v .: "Node") >>= (.: "Address")) <*>
                            ((v .: "Service") >>= (.: "Tags")) <*>
                            ((v .: "Service") >>= (.: "Port")) <*>
                            ((v .: "Checks") >>= mapM (\i -> (,) <$> (i .: "ServiceName") <*> (i .: "Status")))
   parseJSON _          = mzero
