-- | Custom cryptographic hash constructions used by Bitcoin.
module Bitcoin.Hash
  ( -- * Bitcoin constructions
    check32
  , I.hash160
  , I.hash256
  ) where

import qualified Data.ByteString as B
import qualified Bitcoin.Hash.Internal as I

--------------------------------------------------------------------------------

-- | @'check32' == 'B.take' 4 . 'I.hash256'@
check32
  :: B.ByteString  -- ^ Data to hash.
  -> B.ByteString  -- ^ 32-bit long digest.
check32 = B.take 4 . I.hash256
{-# INLINE check32 #-}

