{-# LANGUAGE CPP #-}

-- | This module exports tools for working with Bitcoin keys.
module Bitcoin.Keys
  ( -- * Private
    I.Prv
  , I.parsePrv
  , I.prvRaw
  , I.prvToPub

   -- * Public
  , I.Pub
  , parsePub
  , I.pubCompressed
  , I.pubUncompressed

   -- * Tweak
  , I.Tweak
  , I.parseTweak
  , I.pubAddTweak
  , I.prvAddTweak
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString as B

#ifdef ghcjs_HOST_OS
import qualified Bitcoin.Keys.GHCJS as I
#else
import qualified Bitcoin.Keys.GHC as I
#endif

-- | Construct a 'I.Pub' key from either its compressed or uncompressed SEC-encoded
-- bytes.
--
-- * Compressed keys are 33 bytes. The leftmost byte is @0x02@ if the @y@
-- coordinate is even, or @0x03@ if odd. The remaining 32 bytes
-- are the big-endian encoded @x@ coordinate.
--
-- * Uncompressed keys are 65 bytes. The leftmost byte is @0x04@. The next 32
-- bytes are the big-endian encoded @x@ cordinate. The next 32 bytes are the
-- big-endian encoded @y@ coordinate.
--
-- Returns 'Nothing' if something is not satisfied.
parsePub :: B.ByteString -> Maybe I.Pub
{-# INLINE parsePub #-}
parsePub b = I.parsePubCompressed b <|> parsePubUncompressed b

-- | Builds a public key from its uncompressed SEC-encoded bytes.
--
-- Uncompressed keys are 65 bytes. The leftmost byte is @0x04@. The next 32
-- bytes are the big-endian encoded @x@ cordinate. The next 32 bytes are the
-- big-endian encoded @y@ coordinate.
--
-- Returns 'Nothing' if something is not satisfied.
parsePubUncompressed :: B.ByteString -> Maybe I.Pub
{-# INLINE parsePubUncompressed #-}
parsePubUncompressed b = do
  guard (B.length b == 65 && B.index b 0 == 0x04)
  let w0 = 0x02 + (B.index b 64 .&. 0x01)
  I.parsePubCompressed (B.cons w0 (B.take 32 (B.drop 1 b)))


