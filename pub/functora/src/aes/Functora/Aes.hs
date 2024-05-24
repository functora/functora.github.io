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
    Ikm (..),
    Salt (..),
    Info (..),
    Hkdf (..),
    randomHkdf,
  )
where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Modes as Crypto
import qualified Codec.Utils as Utils
import qualified Crypto.Data.PKCS7 as PKCS7
import Data.Binary (Binary)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal (createAndTrim, memcpy, toForeignPtr)
import qualified Data.Data as Data
import Data.LargeWord
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Functora.AesOrphan ()
import Functora.Prelude
import System.IO.Unsafe (unsafeDupablePerformIO)
import Type.Reflection
import Prelude (fromIntegral)

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
encryptHmac prv@(SomeAesKey cipher _) plain =
  Crypto
    { cryptoValue = value,
      cryptoValueHmac = myHmac prv value
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
unHmacDecrypt prv@(SomeAesKey cipher _) (Crypto value valueHmac) = do
  if valueHmac == myHmac prv value
    then Just ()
    else Nothing
  fmap (from @ByteString @a)
    . PKCS7.unpadBytesN bytesPerBlock
    . blocksToBs
    . Crypto.unCbc AES.decrypt 0 cipher
    $ bsToBlocks value

myHmac ::
  forall a.
  ( From a [Word8],
    From [Word8] a
  ) =>
  SomeAesKey ->
  a ->
  a
myHmac (SomeAesKey _ hmacer) =
  sha256Hmac
    ( from @[Word8] @a
        $ Utils.i2osp
          ( Bits.finiteBitSize hmacer
              `div` Bits.finiteBitSize (0 :: Word8)
          )
          hmacer
    )

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
    someAesKeyHmacer :: a
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

newtype Ikm = Ikm
  { unIkm :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted Ikm

newtype Salt = Salt
  { unSalt :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted Salt

newtype Info = Info
  { unInfo :: ByteString
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving newtype (Binary)
  deriving (Show) via Redacted Info

data Hkdf = Hkdf
  { -- IKM (Input Keying Material): This is the initial input from which you want to derive keys. Typically, the IKM should be at least as long as the output length of the hash function used in the HMAC construction (i.e., the hash function used in HKDF). For instance, if you're using SHA-256, which has a 256-bit output, your IKM should ideally be 256 bits or longer.
    hkdfIkm :: Ikm,
    -- The salt is a non-secret random value that is mixed with the IKM before deriving the keys. It adds an extra layer of security, ensuring that the same IKM with different salts will result in different derived keys. The salt size is not fixed but should be sufficient to ensure uniqueness. A common recommendation is to use a salt that is at least as long as the output length of the hash function. So, if using SHA-256, a 256-bit (32-byte) salt is a reasonable choice.
    hkdfSalt :: Salt,
    -- The info parameter is an optional context or additional data that you can include to derive keys for specific purposes or to differentiate between different applications of the same IKM. The size of the info parameter depends on your use case and how much context you want to provide. It's usually a good practice to keep this as small as possible to avoid unnecessarily inflating the derived key size.
    hkdfCipherInfo :: Info,
    hkdfHmacerInfo :: Info
  }
  deriving stock (Eq, Ord, Read, Data, Generic)
  deriving (Show) via Redacted Hkdf

instance Binary Hkdf

randomHkdf :: (MonadIO m) => Natural -> m Hkdf
randomHkdf size = do
  ikm <- Ikm <$> randomByteString size
  salt <- Salt <$> randomByteString size
  cipherInfo <- Info <$> randomByteString size
  hmacerInfo <- Info <$> randomByteString size
  pure
    Hkdf
      { hkdfIkm = ikm,
        hkdfSalt = salt,
        hkdfCipherInfo = cipherInfo,
        hkdfHmacerInfo = hmacerInfo
      }

drvSomeAesKey ::
  forall word size.
  ( Bounded word,
    Typeable word,
    AES.AESKey word,
    WordByteSize word size,
    Bits.FiniteBits word
  ) =>
  Hkdf ->
  SomeAesKey
drvSomeAesKey hkdf =
  SomeAesKey
    { someAesKeyCipher = derive cipherInfo,
      someAesKeyHmacer = derive hmacerInfo
    }
  where
    ikm = hkdfIkm hkdf
    salt = hkdfSalt hkdf
    cipherInfo = hkdfCipherInfo hkdf
    hmacerInfo = hkdfHmacerInfo hkdf
    derive info =
      unsafeWord @word
        . from @ByteString @[Word8]
        . myHkdf
          (unIkm ikm)
          (unSalt salt)
          (unInfo info)
        . unsafeFrom @Natural @Int
        . natVal
        $ Proxy @size

unsafeWord :: forall a. (Integral a, Bounded a) => [Word8] -> a
unsafeWord =
  fromMaybe (error "unsafeWord overflow failure")
    . safeFromIntegral @Integer @a
    . foldl (\acc i -> acc * 256 + toInteger @Word8 i) (0 :: Integer)

--
-- TODO : This is naive HKDF-SHA256 implementation!!!
-- REWRITE AND TESTS ARE NEEDED!!!
--

-- | perform IO for hashes that do allocation and ffi.
-- unsafeDupablePerformIO is used when possible as the
-- computation is pure and the output is directly linked
-- to the input. we also do not modify anything after it has
-- been returned to the user.
unsafeDoIO :: IO a -> a
unsafeDoIO = unsafeDupablePerformIO

withByteStringPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f =
  withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
  where
    (fptr, off, _) = toForeignPtr b

-- | <https://tools.ietf.org/html/rfc6234 RFC6234>-compatible
-- HKDF-SHA-256 key derivation function.
myHkdf ::
  -- | /IKM/ Input keying material
  ByteString ->
  -- | /salt/ Optional salt value, a non-secret random value (can be @""@)
  ByteString ->
  -- | /info/ Optional context and application specific information (can be @""@)
  ByteString ->
  -- | /L/ length of output keying material in octets (at most 255*32 bytes)
  Int ->
  -- | /OKM/ Output keying material (/L/ bytes)
  ByteString
myHkdf ikm salt info l
  | l == 0 = BS.empty
  | 0 > l || l > 255 * 32 = error "hkdf: invalid L parameter"
  | otherwise = unsafeDoIO $ createAndTrim (32 * fromIntegral cnt) (go 0 BS.empty)
  where
    prk :: ByteString
    prk = sha256Hmac salt ikm
    cnt = fromIntegral ((l + 31) `div` 32) :: Word8
    go :: Word8 -> ByteString -> Ptr Word8 -> IO Int
    go !i t !p
      | i == cnt = return l
      | otherwise = do
          let t' = sha256Hmac prk $ t <> info <> BS.singleton (i + 1)
          withByteStringPtr t' $ \tptr' -> memcpy p tptr' 32
          go (i + 1) t' (p `plusPtr` 32)
