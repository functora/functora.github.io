module Functora.Aes
  ( encrypt,
    decrypt,
    withBlocks,
    SomeAesKey,
    drvSomeAesKey,
  )
where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Modes as Crypto
import qualified Codec.Utils as Crypto
import qualified Crypto.Data.PKCS7 as PKCS7
import qualified Crypto.Hash.SHA256 as SHA256
import Data.LargeWord
import Functora.Prelude
import Type.Reflection

encrypt :: forall a. (From a ByteString, From ByteString a) => SomeAesKey -> a -> a
encrypt (SomeAesKey prv) =
  withBlocks $
    Crypto.cbc AES.encrypt 0 prv

decrypt :: forall a. (From a ByteString, From ByteString a) => SomeAesKey -> a -> a
decrypt (SomeAesKey prv) =
  withBlocks $
    Crypto.unCbc AES.decrypt 0 prv

withBlocks ::
  forall a.
  ( From a ByteString,
    From ByteString a
  ) =>
  ([Word128] -> [Word128]) ->
  a ->
  a
withBlocks expr =
  from @ByteString @a
    . blocksToBs
    . expr
    . bsToBlocks
    . from @a @ByteString

bsToBlocks :: ByteString -> [Word128]
bsToBlocks =
  fmap unsafeWord
    . breakup
    . from @ByteString @[Word8]
    . PKCS7.padBytesN bytesPerBlock
  where
    breakup [] = []
    breakup xs = (take bytesPerBlock xs) : (breakup $ drop bytesPerBlock xs)

blocksToBs :: [Word128] -> ByteString
blocksToBs =
  fromMaybe (error "PKCS7 unpad failure")
    . PKCS7.unpadBytesN bytesPerBlock
    . from @[Word8] @ByteString
    . concat
    . fmap (Crypto.toOctets (256 :: Integer))

bytesPerBlock :: Int
bytesPerBlock = 16

data SomeAesKey = forall a. (AES.AESKey a, Typeable a) => SomeAesKey a

deriving via (Redacted SomeAesKey) instance Show SomeAesKey

instance Eq SomeAesKey where
  (SomeAesKey lhs) == (SomeAesKey rhs)
    | Just HRefl <- typeOf lhs `eqTypeRep` typeOf rhs = lhs == rhs
    | otherwise = False

type family WordByteSizeFamily word where
  WordByteSizeFamily Word128 = 16
  WordByteSizeFamily Word192 = 24
  WordByteSizeFamily Word256 = 32

type WordByteSize word size =
  ( KnownNat size,
    WordByteSizeFamily word ~ size
  )

drvSomeAesKey ::
  forall word {size}.
  ( Bounded word,
    Typeable word,
    AES.AESKey word,
    WordByteSize word size
  ) =>
  Tagged "HkdfIkm" ByteString ->
  Tagged "HkdfSalt" ByteString ->
  Tagged "HkdfInfo" ByteString ->
  SomeAesKey
drvSomeAesKey ikm salt info =
  SomeAesKey
    . unsafeWord @word
    . from @ByteString @[Word8]
    . SHA256.hkdf
      (unTagged @"HkdfIkm" ikm)
      (unTagged @"HkdfSalt" salt)
      (unTagged @"HkdfInfo" info)
    . unsafeFrom @Natural @Int
    . natVal
    $ Proxy @size

unsafeWord :: forall a. (Integral a, Bounded a) => [Word8] -> a
unsafeWord =
  fromMaybe (error "unsafeWord overflow failure")
    . safeFromIntegral @Integer @a
    . foldl (\acc i -> acc * 256 + toInteger @Word8 i) (0 :: Integer)
