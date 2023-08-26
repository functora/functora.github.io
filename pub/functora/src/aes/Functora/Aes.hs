module Functora.Aes
  ( encrypt,
    decrypt,
    SomeAesKey,
    drvSomeAesKey,
    Word128,
    Word192,
    Word256,
  )
where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Modes as Crypto
import qualified Codec.Utils as Crypto
import qualified Crypto.Data.PKCS7 as PKCS7
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Data as Data
import Data.LargeWord
import Functora.Prelude
import Type.Reflection

encrypt :: forall a. (From a ByteString, From ByteString a) => SomeAesKey -> a -> a
encrypt (SomeAesKey prv) =
  from @ByteString @a
    . blocksToBs
    . Crypto.cbc AES.encrypt 0 prv
    . bsToBlocks
    . PKCS7.padBytesN bytesPerBlock
    . from @a @ByteString

decrypt ::
  forall a.
  ( From a ByteString,
    From ByteString a
  ) =>
  SomeAesKey ->
  a ->
  Maybe a
decrypt (SomeAesKey prv) =
  fmap (from @ByteString @a)
    . PKCS7.unpadBytesN bytesPerBlock
    . blocksToBs
    . Crypto.unCbc AES.decrypt 0 prv
    . bsToBlocks
    . from @a @ByteString

bsToBlocks :: ByteString -> [Word128]
bsToBlocks =
  fmap unsafeWord
    . breakup
    . from @ByteString @[Word8]
  where
    breakup [] = []
    breakup xs = (take bytesPerBlock xs) : (breakup $ drop bytesPerBlock xs)

blocksToBs :: [Word128] -> ByteString
blocksToBs =
  from @[Word8] @ByteString
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

instance Data SomeAesKey where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr SomeAesKey"
  dataTypeOf _ = error "TODO : dataTypeOf SomeAesKey"

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
  -- IKM (Input Keying Material): This is the initial input from which you want to derive keys. Typically, the IKM should be at least as long as the output length of the hash function used in the HMAC construction (i.e., the hash function used in HKDF). For instance, if you're using SHA-256, which has a 256-bit output, your IKM should ideally be 256 bits or longer.
  Tagged "IKM" ByteString ->
  -- The salt is a non-secret random value that is mixed with the IKM before deriving the keys. It adds an extra layer of security, ensuring that the same IKM with different salts will result in different derived keys. The salt size is not fixed but should be sufficient to ensure uniqueness. A common recommendation is to use a salt that is at least as long as the output length of the hash function. So, if using SHA-256, a 256-bit (32-byte) salt is a reasonable choice.
  Tagged "Salt" ByteString ->
  -- The info parameter is an optional context or additional data that you can include to derive keys for specific purposes or to differentiate between different applications of the same IKM. The size of the info parameter depends on your use case and how much context you want to provide. It's usually a good practice to keep this as small as possible to avoid unnecessarily inflating the derived key size.
  Tagged "Info" ByteString ->
  SomeAesKey
drvSomeAesKey ikm salt info =
  SomeAesKey
    . unsafeWord @word
    . from @ByteString @[Word8]
    . SHA256.hkdf
      (unTagged @"IKM" ikm)
      (unTagged @"Salt" salt)
      (unTagged @"Info" info)
    . unsafeFrom @Natural @Int
    . natVal
    $ Proxy @size

unsafeWord :: forall a. (Integral a, Bounded a) => [Word8] -> a
unsafeWord =
  fromMaybe (error "unsafeWord overflow failure")
    . safeFromIntegral @Integer @a
    . foldl (\acc i -> acc * 256 + toInteger @Word8 i) (0 :: Integer)
