-- | Cryptographic hash primitives used by Bitcoin.
module Bitcoin.Hash.Prim
  ( -- * Hash
    I.ripemd160
  , I.sha256
    -- * HMAC
  , I.hmacSHA512
  ) where

import qualified Bitcoin.Hash.Internal as I
