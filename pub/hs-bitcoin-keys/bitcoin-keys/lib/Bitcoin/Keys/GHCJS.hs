{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}

-- | Implementation to be used when compiled with GHCJS
module Bitcoin.Keys.GHCJS
  ( Prv
  , parsePrv
  , prvRaw
  , prvToPub
  , prvAddTweak

  , Pub
  , parsePubCompressed
  , pubCompressed
  , pubUncompressed
  , pubAddTweak

  , Tweak
  , parseTweak
  ) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Builder as BB
import qualified GHCJS.Buffer as Buf
import GHCJS.Types (JSVal)
import GHCJS.Nullable
import qualified JavaScript.TypedArray.ArrayBuffer as AB
import qualified JavaScript.TypedArray as TA

--------------------------------------------------------------------------------

-- | Private key.
--
-- Construct with 'parsePrv'.
newtype Prv = Prv B.ByteString
  deriving newtype (Eq, Ord)

-- | Big-endian base-16.
instance Show Prv where
  showsPrec n p = showParen (n > 10) $
    showString "Prv " .
    mappend (BL8.unpack (BB.toLazyByteString (BB.byteStringHex (prvRaw p))))

-- | Obtain the 32 raw bytes inside a 'Prv' (big-endian).
--
-- @
-- 'Just' == 'parsePrv' . 'prvRaw'
-- @
prvRaw :: Prv -> B.ByteString
{-# INLINE prvRaw #-}
prvRaw (Prv x) = x

-- | Construct a 'Prv' key from its raw 32 bytes (big-endian).
--
-- Returns 'Nothing' if something is not satisfied.
--
-- @
-- 'Just' == 'parsePrv' . 'prvRaw'
-- @
parsePrv :: B.ByteString -> Maybe Prv
parsePrv b = do
  guard (B.length b == 32)
  guard (js_prvVerify (bsToU8A b))
  pure (Prv b)

foreign import javascript unsafe
  "h$bitcoin_keys.prvVerify($1)"
  js_prvVerify :: TA.Uint8Array -> Bool

-- | Obtain the 'Pub' key for 'Prv'.
prvToPub :: Prv -> Pub
{-# INLINE prvToPub #-}
prvToPub = Pub . bsFromU8A . js_prvToPub . bsToU8A . prvRaw

foreign import javascript unsafe "h$bitcoin_keys.prvToPub($1)"
  js_prvToPub :: TA.Uint8Array -> TA.Uint8Array

-- | Tweak a 'Prv'ate key by adding 'Tweak' times the generator to it.
--
-- Returns 'Nothing' if the resulting 'Prv' would be invalid.
prvAddTweak :: Tweak -> Prv -> Maybe Prv
{-# INLINE prvAddTweak #-}
prvAddTweak (Tweak t) (Prv p) = do
  jsv <- nullableToMaybe (js_prvAddTweak (bsToU8A p) (bsToU8A t))
  pure $ Prv (bsFromU8A (js_unsafe_JSVal_to_UInt8Array jsv))

foreign import javascript unsafe "h$bitcoin_keys.prvAddTweak($1, $2)"
  js_prvAddTweak :: TA.Uint8Array  -- ^ Private key.
                 -> TA.Uint8Array  -- ^ Tweak.
                           -> Nullable JSVal -- ^ Nullable Uint8Array

--------------------------------------------------------------------------------

-- | Public key.
--
-- Construct with 'Bitcoin.Keys.parsePub'.
newtype Pub = Pub B.ByteString
  deriving newtype (Eq, Ord)

-- | SEC compressed base-16.
instance Show Pub where
  showsPrec n p = showParen (n > 10) $
    showString "Pub " .
    mappend (BL8.unpack (BB.toLazyByteString (BB.byteStringHex (pubCompressed p))))

-- | Obtain the 33-bytes contatining the SEC compressed 'Pub'lic key.
--
-- @
-- 'Just' == 'Bitcoin.Keys.parsePub' . 'pubCompressed'
-- @
pubCompressed :: Pub -> B.ByteString
{-# INLINE pubCompressed #-}
pubCompressed (Pub x) = x

-- | Obtain the 65-bytes contatining the SEC uncompressed 'Pub'lic key.
--
-- @
-- 'Just' == 'Bitcoin.Keys.parsePub' . 'pubUncompressed'
-- @
pubUncompressed :: Pub -> B.ByteString
{-# INLINE pubUncompressed #-}
pubUncompressed = bsFromU8A . js_pubUncompressed . bsToU8A . pubCompressed

foreign import javascript unsafe "h$bitcoin_keys.pubUncompressed($1)"
  js_pubUncompressed :: TA.Uint8Array -- ^ Compressed public key.
                     -> TA.Uint8Array -- ^ Uncompressed public key.


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
parsePubCompressed b = do
  guard (B.length b == 33)
  guard (js_pubVerify (bsToU8A b))
  pure (Pub b)

foreign import javascript unsafe "h$bitcoin_keys.pubVerify($1)"
  js_pubVerify :: TA.Uint8Array   -- ^ Compressed public key.
               -> Bool            -- ^ Whether the key is valid.

-- | Tweak a 'Pub'lic key by adding 'Tweak' times the generator to it.
--
-- Returns 'Nothing' if the resulting 'Pub' would be invalid.
pubAddTweak :: Tweak -> Pub -> Maybe Pub
{-# INLINE pubAddTweak #-}
pubAddTweak (Tweak t) (Pub p) = do
  jsv <- nullableToMaybe (js_pubAddTweak (bsToU8A p) (bsToU8A t))
  pure $ Pub (bsFromU8A (js_unsafe_JSVal_to_UInt8Array jsv))

foreign import javascript unsafe "h$bitcoin_keys.pubAddTweak($1, $2)"
  js_pubAddTweak :: TA.Uint8Array  -- ^ Public key.
                 -> TA.Uint8Array  -- ^ Tweak.
                 -> Nullable JSVal -- ^ Nullable Uint8Array

--------------------------------------------------------------------------------

-- | A 32-byte number used to modify a 'Pub' or 'Prv' using 'prvAddTweak'
-- or 'pubAddTweak'.
newtype Tweak = Tweak B.ByteString
  deriving newtype (Eq, Ord)

-- | Big-endian base-16.
instance Show Tweak where
  showsPrec n (Tweak b) = showParen (n > 10) $
    showString "Tweak " .
    mappend (BL8.unpack (BB.toLazyByteString (BB.byteStringHex b)))

-- | Construct a 'Tweak' from its raw 32 bytes (big-endian).
--
-- Returns 'Nothing' if something is not satisfied.
parseTweak :: B.ByteString -> Maybe Tweak
parseTweak b = do
  guard (B.length b == 32)
  pure $ Tweak b

--------------------------------------------------------------------------------

bsToU8A :: B.ByteString -> TA.Uint8Array
bsToU8A x
  | B.length x == 0 = js_emptyUint8Array
  | otherwise = let (buf, off, len) = Buf.fromByteString x
                in js_newUint8Array (Buf.getArrayBuffer buf) off len

foreign import javascript unsafe "new Uint8Array($1, $2, $3)"
  js_newUint8Array :: AB.ArrayBuffer
                   -> Int  -- ^ Byte offset.
                   -> Int  -- ^ Byte length.
                   -> TA.Uint8Array

foreign import javascript unsafe "new Uint8Array(0)"
  js_emptyUint8Array :: TA.Uint8Array

--------------------------------------------------------------------------------

bsFromU8A :: TA.Uint8Array -> B.ByteString
bsFromU8A x = Buf.toByteString (TA.byteOffset x)
                               (Just (TA.byteLength x))
                               (Buf.createFromArrayBuffer (TA.buffer x))

--------------------------------------------------------------------------------

foreign import javascript unsafe "$r = $1;"
  js_unsafe_JSVal_to_UInt8Array :: JSVal -> TA.Uint8Array

