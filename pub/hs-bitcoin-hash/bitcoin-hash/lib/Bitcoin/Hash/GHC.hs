{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Implementation to be used when compiled with GHC
module Bitcoin.Hash.GHC
  ( hash160
  , hash256
  , ripemd160
  , sha256
  , hmacSHA512
  ) where

import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B

--------------------------------------------------------------------------------

-- | @'hash160' == 'ripemd160' . 'sha256'@
hash160
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 160-bit long digest.
{-# INLINE hash160 #-}
hash160 = BA.convert
        . Hash.hash @(Hash.Digest Hash.SHA256) @Hash.RIPEMD160
        . Hash.hash @B.ByteString @Hash.SHA256

--------------------------------------------------------------------------------

-- | @'hash256' == 'sha256' . 'sha256'@
hash256
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 256-bit long digest.
{-# INLINE hash256 #-}
hash256 = BA.convert
        . Hash.hash @(Hash.Digest Hash.SHA256) @Hash.SHA256
        . Hash.hash @B.ByteString @Hash.SHA256

--------------------------------------------------------------------------------

ripemd160
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 20-byte long digest.
{-# INLINE ripemd160 #-}
ripemd160 = BA.convert
          . Hash.hash @B.ByteString @Hash.RIPEMD160

--------------------------------------------------------------------------------

sha256
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 32-byte long digest.
{-# INLINE sha256 #-}
sha256 = BA.convert
       . Hash.hash @B.ByteString @Hash.SHA256

--------------------------------------------------------------------------------

hmacSHA512
  :: B.ByteString  -- ^ Key.
  -> B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 64-byte long digest.
{-# INLINE hmacSHA512 #-}
hmacSHA512 k d = BA.convert (HMAC.hmac k d :: HMAC.HMAC Hash.SHA512)

