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
       ( Nonce ()
       , Secret ()
       , Message
       , Signature ()
       , sign
       , verify
       , genNonce
       , genSecret
       , nextNonce
       , initNonce
       , initSecret
       , initSignature
       ) where

import           Data.Word
import           Foreign.C
import           Foreign.Ptr
import           System.Entropy
import qualified Data.ByteString as B
import           Foreign.Marshal
import           Foreign.Storable
import           System.IO.Unsafe
import           Data.ByteString.Unsafe

newtype Nonce  = Nonce B.ByteString

newtype Secret = Secret B.ByteString

newtype Signature = Signature B.ByteString

type Message   = B.ByteString

nonceSize :: Int
nonceSize = 16

sigSize :: Int
sigSize = 16

secretSize :: Int
secretSize = 32

initSecret :: B.ByteString -> Maybe Secret
initSecret secret
  | B.length secret /= secretSize = Nothing
  | otherwise                     =
      Just $ Secret $ unsafePerformIO $
        B.useAsCString secret $ \secretPtr -> do
          c_poly1305_clamp secretPtr
          B.packCStringLen (secretPtr, secretSize)

initSignature :: B.ByteString -> Maybe Signature
initSignature sig
  | B.length sig /= sigSize = Nothing
  | otherwise               = Just $ Signature sig

initNonce :: B.ByteString -> Maybe Nonce
initNonce nonce
  | B.length nonce /= nonceSize = Nothing
  | otherwise                   = Just $ Nonce nonce

genSecret :: IO Secret
genSecret = fmap Secret $ getEntropy secretSize

genNonce :: IO Nonce
genNonce = fmap Nonce $ getEntropy nonceSize

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

verify :: Secret -> Nonce -> Message -> Signature -> Bool
verify (Secret secret) (Nonce nonce) msg (Signature sig) = unsafePerformIO $
  B.useAsCString secret $ \secretPtr ->
    unsafeUseAsCString nonce $ \noncePtr ->
      unsafeUseAsCStringLen msg $ \(msgPtr, msgLen) ->
        unsafeUseAsCString sig $ \sigPtr -> do
          c_poly1305_clamp secretPtr
          return (0 /= c_poly1305_verify sigPtr secretPtr noncePtr msgPtr (fromIntegral msgLen))

sign :: Secret -> Nonce -> Message -> Signature
sign (Secret secret) (Nonce nonce) msg = unsafePerformIO $
  B.useAsCString secret $ \secretPtr ->
    unsafeUseAsCString nonce $ \noncePtr ->
      unsafeUseAsCStringLen msg $ \(msgPtr, msgLen) -> do
        sigPtr <- mallocBytes nonceSize
        c_poly1305_clamp secretPtr
        c_poly1305_authenticate sigPtr secretPtr noncePtr msgPtr (fromIntegral msgLen)
        fmap Signature $ unsafePackMallocCStringLen (sigPtr, sigSize)

foreign import capi safe "poly1305aes/poly1305aes.h poly1305aes_verify"
  c_poly1305_verify :: CString -> CString -> CString -> CString -> CUInt -> CInt

foreign import capi safe "poly1305aes/poly1305aes.h poly1305aes_clamp"
  c_poly1305_clamp :: CString -> IO ()

foreign import capi safe "poly1305aes/poly1305aes.h poly1305aes_authenticate"
  c_poly1305_authenticate :: CString -> CString -> CString -> CString -> CUInt -> IO ()
