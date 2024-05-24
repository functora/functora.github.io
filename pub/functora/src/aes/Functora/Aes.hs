{-# LANGUAGE BangPatterns #-}

module Functora.Aes
  ( Crypto,
    encryptHmac,
    unHmacDecrypt,
    SomeAesKey,
    drvSomeAesKey,
    Word128,
    Word192,
    Word256,
    Km (..),
    randomKm,
  )
where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Modes as Crypto
import qualified Codec.Utils as Utils
import qualified Crypto.Data.PKCS7 as PKCS7
import qualified Data.Bits as Bits
import qualified Data.Data as Data
import Data.LargeWord
import Functora.AesOrphan ()
import Functora.Prelude
import Type.Reflection

data Crypto a = Crypto
  { cryptoValue :: a,
    cryptoValueHmac :: a
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

instance (Binary a) => Binary (Crypto a)

encryptHmac ::
  forall a.
  ( From a ByteString,
    From a [Word8],
    From [Word8] a
  ) =>
  SomeAesKey ->
  a ->
  Crypto a
encryptHmac (SomeAesKey cipher hmacer) plain =
  Crypto
    { cryptoValue = value,
      cryptoValueHmac = sha256Hmac (unOkm hmacer) value
    }
  where
    value =
      from @[Word8] @a
        . blocksToBs
        . Crypto.cbc AES.encrypt 0 cipher
        . bsToBlocks
        . PKCS7.padBytesN bytesPerBlock
        $ from @a @ByteString plain

unHmacDecrypt ::
  forall a.
  ( Eq a,
    From ByteString a,
    From a [Word8],
    From [Word8] a
  ) =>
  SomeAesKey ->
  Crypto a ->
  Maybe a
unHmacDecrypt (SomeAesKey cipher hmacer) (Crypto value valueHmac) = do
  if sha256Hmac (unOkm hmacer) value == valueHmac
    then Just ()
    else Nothing
  fmap (from @ByteString @a)
    . PKCS7.unpadBytesN bytesPerBlock
    . blocksToBs
    . Crypto.unCbc AES.decrypt 0 cipher
    $ bsToBlocks value

bsToBlocks :: forall a. (From a [Word8]) => a -> [Word128]
bsToBlocks =
  fmap unsafeWord
    . breakup
    . from @a @[Word8]
  where
    breakup [] = []
    breakup xs = (take bytesPerBlock xs) : (breakup $ drop bytesPerBlock xs)

blocksToBs :: forall a. (From [Word8] a) => [Word128] -> a
blocksToBs =
  from @[Word8] @a
    . concat
    . fmap (Utils.toOctets (256 :: Integer))

bytesPerBlock :: Int
bytesPerBlock = 16

data SomeAesKey = forall a.
  ( Typeable a,
    AES.AESKey a,
    Bits.FiniteBits a
  ) =>
  SomeAesKey
  { someAesKeyCipher :: a,
    someAesKeyHmacer :: Okm
  }

deriving via Redacted SomeAesKey instance Show SomeAesKey

instance Eq SomeAesKey where
  (SomeAesKey xc xh) == (SomeAesKey yc yh) =
    case (,) <$> cmp xc yc <*> cmp xh yh of
      Just (HRefl, HRefl) -> (xc == yc) && (xh == yh)
      Nothing -> False
    where
      cmp lhs rhs = typeOf lhs `eqTypeRep` typeOf rhs

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

data Km = Km
  { -- IKM (Input Keying Material): This is the initial input from which you want to derive keys. Typically, the IKM should be at least as long as the output length of the hash function used in the HMAC construction (i.e., the hash function used in HKDF). For instance, if you're using SHA-256, which has a 256-bit output, your IKM should ideally be 256 bits or longer.
    kmIkm :: Ikm,
    -- The salt is a non-secret random value that is mixed with the IKM before deriving the keys. It adds an extra layer of security, ensuring that the same IKM with different salts will result in different derived keys. The salt size is not fixed but should be sufficient to ensure uniqueness. A common recommendation is to use a salt that is at least as long as the output length of the hash function. So, if using SHA-256, a 256-bit (32-byte) salt is a reasonable choice.
    kmSalt :: SaltKm,
    -- The info parameter is an optional context or additional data that you can include to derive keys for specific purposes or to differentiate between different applications of the same IKM. The size of the info parameter depends on your use case and how much context you want to provide. It's usually a good practice to keep this as small as possible to avoid unnecessarily inflating the derived key size.
    kmCipherInfo :: InfoKm,
    kmHmacerInfo :: InfoKm
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving (Show) via Redacted Km

instance Binary Km

randomKm :: (MonadIO m) => Natural -> m Km
randomKm size = do
  ikm <- Ikm <$> randomByteString size
  salt <- SaltKm <$> randomByteString size
  cipherInfo <- InfoKm <$> randomByteString size
  hmacerInfo <- InfoKm <$> randomByteString size
  pure
    Km
      { kmIkm = ikm,
        kmSalt = salt,
        kmCipherInfo = cipherInfo,
        kmHmacerInfo = hmacerInfo
      }

drvSomeAesKey ::
  forall word size.
  ( Bounded word,
    Typeable word,
    AES.AESKey word,
    WordByteSize word size,
    Bits.FiniteBits word
  ) =>
  Km ->
  SomeAesKey
drvSomeAesKey km =
  SomeAesKey
    { someAesKeyCipher =
        unsafeWord @word
          . from @ByteString @[Word8]
          . unOkm
          $ derive cipherInfo,
      someAesKeyHmacer =
        derive hmacerInfo
    }
  where
    ikm = kmIkm km
    salt = kmSalt km
    cipherInfo = kmCipherInfo km
    hmacerInfo = kmHmacerInfo km
    derive info =
      sha256Hkdf
        ikm
        salt
        info
        . OkmByteSize
        . natVal
        $ Proxy @size

unsafeWord :: forall a. (Integral a, Bounded a) => [Word8] -> a
unsafeWord =
  fromMaybe (error "unsafeWord overflow failure")
    . safeFromIntegral @Integer @a
    . foldl (\acc i -> acc * 256 + toInteger @Word8 i) (0 :: Integer)
