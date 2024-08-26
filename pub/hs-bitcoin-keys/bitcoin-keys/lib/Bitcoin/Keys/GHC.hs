{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Implementation to be used when compiled with GHC
module Bitcoin.Keys.GHC
  ( Prv,
    parsePrv,
    prvRaw,
    prvToPub,
    prvAddTweak,
    Pub,
    parsePubCompressed,
    pubCompressed,
    pubUncompressed,
    pubAddTweak,
    Tweak,
    parseTweak,
    Sig (..),
    mkSigDer,
    unSigDer,
  )
where

import Control.Monad
import qualified Crypto.Secp256k1 as K
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified System.IO.Unsafe as Unsafe

--------------------------------------------------------------------------------

-- | Private key.
--
-- Construct with 'parsePrv'.
newtype Prv = Prv K.SecKey
  deriving newtype (Eq)

instance Ord Prv where
  compare a b = compare (prvRaw a) (prvRaw b)
  {-# INLINE compare #-}

-- | Big-endian base-16.
instance Show Prv where
  showsPrec n p =
    showParen (n > 10) $
      showString "Prv "
        . mappend (BL8.unpack (BB.toLazyByteString (BB.byteStringHex (prvRaw p))))

-- | Obtain the 32 raw bytes inside a 'Prv' (big-endian).
--
-- @
-- 'Just' == 'parsePrv' . 'prvRaw'
-- @
prvRaw :: Prv -> B.ByteString
{-# INLINE prvRaw #-}
prvRaw (Prv x) = getSecKey x

#if MIN_VERSION_secp256k1_haskell(1,0,0)
getSecKey :: K.SecKey -> B.ByteString
getSecKey (K.SecKey x) = x
#else
getSecKey = K.getSecKey
#endif

-- | Construct a 'Prv' key from its raw 32 bytes (big-endian).
--
-- Returns 'Nothing' if something is not satisfied.
--
-- @
-- 'Just' == 'parsePrv' . 'prvRaw'
-- @
parsePrv :: B.ByteString -> Maybe Prv
{-# INLINE parsePrv #-}
parsePrv x = do
  guard (B.length x == 32)
  Prv <$> K.secKey x

-- | Obtain the 'Pub' key for 'Prv'.
prvToPub :: Prv -> Pub
{-# INLINE prvToPub #-}
prvToPub (Prv x) = Pub $ (withCtx K.derivePubKey) x

-- | Tweak a 'Prv'ate key by adding 'Tweak' times the generator to it.
--
-- Returns 'Nothing' if the resulting 'Prv' would be invalid.
--
-- @
-- 'pubAddTweak' t . 'prvToPub' == fmap 'prvToPub' . 'prvAddTweak' t
-- @
prvAddTweak :: Tweak -> Prv -> Maybe Prv
{-# INLINE prvAddTweak #-}
prvAddTweak (Tweak t) (Prv p) = Prv <$> (withCtx K.tweakAddSecKey) p t

--------------------------------------------------------------------------------

-- | Public key.
--
-- Construct with 'Bitcoin.Keys.parsePub'.
newtype Pub = Pub K.PubKey
  deriving newtype (Eq)

instance Ord Pub where
  compare a b = compare (pubCompressed a) (pubCompressed b)
  {-# INLINE compare #-}

-- | SEC compressed base-16.
instance Show Pub where
  showsPrec n p =
    showParen (n > 10) $
      showString "Pub "
        . mappend (BL8.unpack (BB.toLazyByteString (BB.byteStringHex (pubCompressed p))))

-- | Obtain the 33-bytes contatining the SEC compressed 'Pub'lic key.
--
-- @
-- 'Just' == 'Bitcoin.Keys.parsePub' . 'pubCompressed'
-- @
pubCompressed :: Pub -> B.ByteString
{-# INLINE pubCompressed #-}
pubCompressed (Pub x) = (withCtx K.exportPubKey) True x

-- | Obtain the 65-bytes contatining the SEC uncompressed 'Pub'lic key.
--
-- @
-- 'Just' == 'Bitcoin.Keys.parsePub' . 'pubUncompressed'
-- @
pubUncompressed :: Pub -> B.ByteString
{-# INLINE pubUncompressed #-}
pubUncompressed (Pub x) = (withCtx K.exportPubKey) False x

-- | Builds a public key from its compressed SEC-encoded bytes.
--
-- * Compressed keys are 33 bytes. The leftmost byte is @0x02@ if the @y@
-- coordinate is even, or @0x03@ if odd. The remaining 32 bytes
-- are the big-endian encoded @x@ coordinate.
--
-- @
-- 'Just' == 'Bitcoin.Keys.parsePub' . 'pubCompressed'
-- @
parsePubCompressed :: B.ByteString -> Maybe Pub
{-# INLINE parsePubCompressed #-}
parsePubCompressed x = do
  guard (B.length x == 33)
  Pub <$> (withCtx K.importPubKey) x

-- | Tweak a 'Pub'lic key by adding 'Tweak' times the generator to it.
--
-- Returns 'Nothing' if the resulting 'Pub' would be invalid.
--
-- @
-- 'pubAddTweak' t . 'prvToPub' == fmap 'prvToPub' . 'prvAddTweak' t
-- @
pubAddTweak :: Tweak -> Pub -> Maybe Pub
{-# INLINE pubAddTweak #-}
pubAddTweak (Tweak t) (Pub p) = Pub <$> (withCtx K.tweakAddPubKey) p t

--------------------------------------------------------------------------------

-- | A 32-byte number used to modify a 'Pub' or 'Prv' using 'prvAddTweak'
-- or 'pubAddTweak'.
newtype Tweak = Tweak K.Tweak
  deriving newtype (Eq)

instance Ord Tweak where
  compare (Tweak a) (Tweak b) = compare (getTweak a) (getTweak b)

#if MIN_VERSION_secp256k1_haskell(1,0,0)
getTweak :: K.Tweak -> B.ByteString
getTweak (K.Tweak x) = x
#else
getTweak = K.getTweak
#endif

-- | Big-endian base-16.
instance Show Tweak where
  showsPrec n (Tweak x) =
    showParen (n > 10) $
      showString "Tweak "
        . mappend
          ( BL8.unpack (BB.toLazyByteString (BB.byteStringHex (getTweak x)))
          )

-- | Construct a 'Tweak' from its raw 32 bytes (big-endian).
--
-- Returns 'Nothing' if something is not satisfied.
parseTweak :: B.ByteString -> Maybe Tweak
{-# INLINE parseTweak #-}
parseTweak x = do
  guard (B.length x == 32)
  Tweak <$> K.tweak x

newtype Sig = Sig
  { unSig :: K.Sig
  }
  deriving stock (Eq, Show)

mkSigDer :: B.ByteString -> Maybe Sig
mkSigDer =
  fmap Sig . withCtx K.importSig

unSigDer :: Sig -> B.ByteString
unSigDer =
  withCtx K.exportSig . unSig

#if MIN_VERSION_secp256k1_haskell(1,0,0)
withCtx :: (K.Ctx -> a) -> a
withCtx f = Unsafe.unsafePerformIO $ K.withContext (pure . f)
#else
withCtx :: a -> a
withCtx = id
#endif
