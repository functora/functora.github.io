{-# LANGUAGE RecordWildCards #-}

module Functora.Qr
  ( -- * Image
    Border (..),
    Scale (..),
    qrToBmp,

    -- * URL
    qrToBmpDataUrlBL,
    qrToBmpDataUrlTL,
  )
where

import Codec.BMP (BMP)
import qualified Codec.BMP as BMP
import Codec.QRCode (QRImage (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Unboxed as UV
import Functora.Prelude

newtype Border = Border
  { unBorder :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype (Num, Enum, Real, Integral)

newtype Scale = Scale
  { unScale :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype (Num, Enum, Real, Integral)

xff :: Word8
xff = 0xff

x00 :: Word8
x00 = 0x00

-- | Convert the QR code into an BMP image.
qrToBmp ::
  -- | Border to add around the QR code, recommended is 4 (<0 is treated as 0)
  Border ->
  -- | Factor to scale the image (<1 is treated as 1)
  Scale ->
  -- | The QRImage
  QRImage ->
  BMP
qrToBmp border scale QRImage {..}
  | border <= 0 && scale <= 1 =
      BMP.packRGBA32ToBMP
        qrImageSize
        qrImageSize
        . BS.pack
        . (>>= (\x -> [x, x, x, xff]))
        $ map (bool xff x00) (UV.toList qrImageData)
qrToBmp border' scale' QRImage {..} =
  let border = border' `max` 0
      scale = scale' `max` 1
      size = (qrImageSize + 2 * (unBorder border)) * (unScale scale)
   in BMP.packRGBA32ToBMP
        size
        size
        . BS.pack
        . (>>= (\x -> [x, x, x, xff]))
        . concat
        . doScale scale
        . addBorder border
        $ toMatrix qrImageData
  where
    toMatrix :: UV.Vector Bool -> [[Word8]]
    toMatrix img
      | UV.null img = []
      | otherwise =
          let (h, t) = UV.splitAt qrImageSize img
           in map (bool xff x00) (UV.toList h) : toMatrix t
    addBorder :: Border -> [[Word8]] -> [[Word8]]
    addBorder 0 img = img
    addBorder (Border n) img = topBottom ++ addLeftRight img ++ topBottom
      where
        topBottom = [replicate ((qrImageSize + 2 * n) * n) xff]
        leftRight = replicate n xff
        addLeftRight = map (\x -> leftRight ++ x ++ leftRight)
    doScale :: Scale -> [[Word8]] -> [[Word8]]
    doScale 1 img = img
    doScale (Scale n) img = scaleV img
      where
        scaleV :: [[Word8]] -> [[Word8]]
        scaleV = concatMap (replicate n . scaleH)
        scaleH :: [Word8] -> [Word8]
        scaleH = concatMap (replicate n)

-- | Convert an QR code into a bytestring-like Uri.
--   Has the same arguments as `qrToBmp`.
--   This can be used to display a image in HTML without creating a temporary file.
qrToBmpDataUrlBL ::
  forall a.
  ( From BL.ByteString a
  ) =>
  Border ->
  Scale ->
  QRImage ->
  a
qrToBmpDataUrlBL border scale =
  from @BL.ByteString @a
    . ("data:image/bmp;base64," <>)
    . B64L.encode
    . BMP.renderBMP
    . qrToBmp border scale

-- | Convert an QR code into a text-like Uri.
--   Has the same arguments as `qrToBmp`.
--   This can be used to display a image in HTML without creating a temporary file.
qrToBmpDataUrlTL ::
  forall a.
  ( From TL.Text a
  ) =>
  Border ->
  Scale ->
  QRImage ->
  a
{-# INLINE qrToBmpDataUrlTL #-}
qrToBmpDataUrlTL border scale =
  from @TL.Text @a
    . TL.pack
    . BLC8.unpack
    . qrToBmpDataUrlBL border scale
