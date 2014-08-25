{-# LANGUAGE CApiFFI #-}

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

module Leela.Data.Signature
       ( MAC ()
       , Nonce ()
       , Secret ()
       , Message
       , sign
       , verify
       , genNonce
       , genSecret
       , nextNonce
       , initNonce
       , initSecret
       , initMAC
       ) where

import           Data.Word
import           Foreign.C
import           Foreign.Ptr
import           System.Entropy
import qualified Data.ByteString as B
import           Foreign.Marshal
import           Foreign.Storable
import           System.IO.Unsafe
import           Control.Applicative
import           Data.ByteString.Unsafe

newtype Nonce = Nonce B.ByteString
              deriving (Eq, Ord, Show)

newtype Secret = Secret B.ByteString
               deriving (Eq, Ord, Show)

newtype MAC = MAC B.ByteString
            deriving (Eq, Ord, Show)

type Message = B.ByteString

nonceSize :: Int
nonceSize = 16

sigSize :: Int
sigSize = 16

secretSize :: Int
secretSize = 32

initSecret :: B.ByteString -> Secret
initSecret secret =
  Secret $ unsafePerformIO $
    B.useAsCString sizedSecret $ \secretPtr -> do
      c_poly1305_clamp secretPtr
      B.packCStringLen (secretPtr, secretSize)
    where
      secretLen = B.length secret

      sizedSecret
        | secretLen < secretSize = B.concat [secret, B.replicate (secretSize - secretLen) 0]
        | otherwise              = secret

initMAC :: B.ByteString -> Maybe MAC
initMAC sig
  | B.length sig /= sigSize = Nothing
  | otherwise               = Just $ MAC sig

initNonce :: B.ByteString -> Maybe Nonce
initNonce nonce
  | B.length nonce /= nonceSize = Nothing
  | otherwise                   = Just $ Nonce nonce

genSecret :: IO Secret
genSecret = Secret <$> getEntropy secretSize

genNonce :: IO Nonce
genNonce = Nonce <$> getEntropy nonceSize

nextNonce :: Nonce -> Nonce
nextNonce (Nonce nonce) = unsafePerformIO $
  unsafeUseAsCString nonce $ \noncePtr -> do
    wnonce0 <- peek (castPtr noncePtr)
    wnonce1 <- peekByteOff (castPtr noncePtr) (nonceSize `div` 2)
    fmap Nonce (buildNonce wnonce0 wnonce1)

    where
      buildNonce :: Word64 -> Word64 -> IO B.ByteString
      buildNonce wnonce0 wnonce1 = do
        let nextNounce0 = wnonce0 + 1
            nextNounce1 = if wnonce0 == 0 then wnonce1 + 1 else wnonce1
        noncePtr <- mallocBytes nonceSize
        poke noncePtr nextNounce0
        pokeByteOff noncePtr (nonceSize `div` 2) nextNounce1
        unsafePackMallocCStringLen (castPtr noncePtr, nonceSize)

verify :: Secret -> Nonce -> Message -> MAC -> Bool
verify (Secret secret) (Nonce nonce) msg (MAC sig) = unsafePerformIO $
  unsafeUseAsCString secret $ \secretPtr ->
    unsafeUseAsCString nonce $ \noncePtr ->
      unsafeUseAsCStringLen msg $ \(msgPtr, msgLen) ->
        unsafeUseAsCString sig $ \sigPtr ->
          fmap (0 /=) (c_poly1305_verify sigPtr secretPtr noncePtr msgPtr (fromIntegral msgLen))

sign :: Secret -> Nonce -> Message -> MAC
sign (Secret secret) (Nonce nonce) msg = unsafePerformIO $
  unsafeUseAsCString secret $ \secretPtr ->
    unsafeUseAsCString nonce $ \noncePtr ->
      unsafeUseAsCStringLen msg $ \(msgPtr, msgLen) -> do
        sigPtr <- mallocBytes nonceSize
        c_poly1305_authenticate sigPtr secretPtr noncePtr msgPtr (fromIntegral msgLen)
        MAC <$> unsafePackMallocCStringLen (sigPtr, sigSize)

foreign import capi unsafe "poly1305aes/poly1305aes.h poly1305aes_verify"
  c_poly1305_verify :: CString -> CString -> CString -> CString -> CUInt -> IO CInt

foreign import capi unsafe "poly1305aes/poly1305aes.h poly1305aes_clamp"
  c_poly1305_clamp :: CString -> IO ()

foreign import capi unsafe "poly1305aes/poly1305aes.h poly1305aes_authenticate"
  c_poly1305_authenticate :: CString -> CString -> CString -> CString -> CUInt -> IO ()
