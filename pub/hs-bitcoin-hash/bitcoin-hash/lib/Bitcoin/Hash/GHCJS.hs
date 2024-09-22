{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Implementation to be used when compiled with GHCJS
module Bitcoin.Hash.GHCJS
  ( hash160
  , hash256
  , ripemd160
  , sha256
  , hmacSHA512
  ) where

import qualified Data.ByteString as B
import qualified GHCJS.Buffer as Buf
import qualified JavaScript.TypedArray.ArrayBuffer as AB
import qualified JavaScript.TypedArray as TA

--------------------------------------------------------------------------------

-- | @'hash160' == 'ripemd160' . 'sha256'@
hash160
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 160-bit long digest.
{-# INLINE hash160 #-}
hash160 = byteStringFromUint8Array
        . js_ripemd160
        . js_sha256
        . byteStringToUint8Array

--------------------------------------------------------------------------------

-- | @'hash256' == 'sha256' . 'sha256'@
hash256
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 256-bit long digest.
{-# INLINE hash256 #-}
hash256 = byteStringFromUint8Array
        . js_sha256
        . js_sha256
        . byteStringToUint8Array

--------------------------------------------------------------------------------

-- | RIPEMD-160
ripemd160
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 160-bits long digest.
{-# INLINE ripemd160 #-}
ripemd160 = byteStringFromUint8Array . js_ripemd160 . byteStringToUint8Array

foreign import javascript unsafe "h$bitcoin_hash.ripemd160($1)"
  js_ripemd160 :: TA.Uint8Array -> TA.Uint8Array

--------------------------------------------------------------------------------

-- | SHA256
sha256
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 256-bits long digest.
{-# INLINE sha256 #-}
sha256 = byteStringFromUint8Array . js_sha256 . byteStringToUint8Array

foreign import javascript unsafe "h$bitcoin_hash.sha256($1)"
  js_sha256 :: TA.Uint8Array -> TA.Uint8Array

--------------------------------------------------------------------------------

-- | HMAC-SHA512
hmacSHA512
  :: B.ByteString  -- ^ Arbitrary length key.
  -> B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 64-byte long digest.
{-# INLINE hmacSHA512 #-}
hmacSHA512 k d =
  byteStringFromUint8Array $ js_hmacSHA512 (byteStringToUint8Array k)
                                           (byteStringToUint8Array d)

foreign import javascript unsafe "h$bitcoin_hash.hmacSHA512($1, $2)"
  js_hmacSHA512 :: TA.Uint8Array -> TA.Uint8Array -> TA.Uint8Array

--------------------------------------------------------------------------------

byteStringToUint8Array :: B.ByteString -> TA.Uint8Array
{-# INLINE byteStringToUint8Array #-}
byteStringToUint8Array = \x -> case B.length x of
  0 -> js_emptyUint8Array
  _ -> let (buf, off, len) = Buf.fromByteString x
       in js_newUint8Array (Buf.getArrayBuffer buf) off len

foreign import javascript unsafe "new Uint8Array($1, $2, $3)"
  js_newUint8Array :: AB.ArrayBuffer -> Int -> Int -> TA.Uint8Array

foreign import javascript unsafe "new Uint8Array(0)"
  js_emptyUint8Array :: TA.Uint8Array

--------------------------------------------------------------------------------

byteStringFromUint8Array :: TA.Uint8Array -> B.ByteString
{-# INLINE byteStringFromUint8Array #-}
byteStringFromUint8Array = \x ->
  Buf.toByteString (TA.byteOffset x)
                   (Just (TA.byteLength x))
                   (Buf.createFromArrayBuffer (TA.buffer x))

