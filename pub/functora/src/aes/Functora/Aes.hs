module Functora.Aes
  ( encrypt,
    decrypt,
    withWs128,
    PrvKey,
    drvPrvKey,
    drvPrvKeyW128,
    bsToWs128,
    ws128ToBs,
  )
where

import qualified Codec.Encryption.AES as AES
import qualified Codec.Encryption.Modes as Crypto
import qualified Codec.Utils as Crypto
import qualified Crypto.Data.PKCS7 as PKCS7
import qualified Crypto.Hash.SHA256 as SHA256
import Data.LargeWord
import Functora.Prelude

encrypt ::
  forall a.
  ( From a ByteString,
    From ByteString a
  ) =>
  PrvKey Word128 ->
  a ->
  a
encrypt prv =
  withWs128 $
    Crypto.cbc AES.encrypt 0 (unPrvKey prv)

decrypt ::
  forall a.
  ( From a ByteString,
    From ByteString a
  ) =>
  PrvKey Word128 ->
  a ->
  a
decrypt prv =
  withWs128 $
    Crypto.unCbc AES.decrypt 0 (unPrvKey prv)

withWs128 ::
  forall a.
  ( From a ByteString,
    From ByteString a
  ) =>
  ([Word128] -> [Word128]) ->
  a ->
  a
withWs128 expr =
  from @ByteString @a
    . ws128ToBs
    . expr
    . bsToWs128
    . from @a @ByteString

newtype PrvKey a = PrvKey
  { unPrvKey :: a
  }
  deriving newtype (Eq, Ord, Read)
  deriving (Show) via Redacted (PrvKey a)
  deriving stock (Data, Generic)

drvPrvKey ::
  (ByteString -> a) ->
  Tagged "HkdfLength" Int ->
  Tagged "HkdfSalt" ByteString ->
  Tagged "HkdfInfo" ByteString ->
  Tagged "HkdfIkm" ByteString ->
  PrvKey a
drvPrvKey cons size salt info ikm =
  PrvKey . cons $
    SHA256.hkdf
      (unTagged @"HkdfIkm" ikm)
      (unTagged @"HkdfSalt" salt)
      (unTagged @"HkdfInfo" info)
      (unTagged @"HkdfLength" size)

drvPrvKeyW128 ::
  Tagged "HkdfSalt" ByteString ->
  Tagged "HkdfInfo" ByteString ->
  Tagged "HkdfIkm" ByteString ->
  PrvKey Word128
drvPrvKeyW128 =
  drvPrvKey
    (unsafeWord128 . from @ByteString @[Word8])
    (Tagged @"HkdfLength" 16)

bsToWs128 :: ByteString -> [Word128]
bsToWs128 =
  fmap unsafeWord128
    . breakup
    . from @ByteString @[Word8]
    . PKCS7.padBytesN bytesPerBlock
  where
    breakup [] = []
    breakup xs = (take bytesPerBlock xs) : (breakup $ drop bytesPerBlock xs)

ws128ToBs :: [Word128] -> ByteString
ws128ToBs =
  fromMaybe (error "PKCS7 unpad failure")
    . PKCS7.unpadBytesN bytesPerBlock
    . from @[Word8] @ByteString
    . concat
    . fmap (Crypto.toOctets (256 :: Integer))

bytesPerBlock :: Int
bytesPerBlock = 16

unsafeWord128 :: [Word8] -> Word128
unsafeWord128 =
  fromMaybe (error "Word128 overflow failure")
    . safeFromIntegral @Integer @Word128
    . foldl (\acc i -> acc * 256 + toInteger @Word8 i) (0 :: Integer)
