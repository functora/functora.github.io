{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.QrOrphan () where

import Codec.BMP
  ( BMP (..),
    BitmapInfo (..),
    BitmapInfoV3 (..),
    BitmapInfoV4 (..),
    BitmapInfoV5 (..),
    CIEXYZ (..),
    Compression (..),
    FileHeader (..),
  )
import Codec.QRCode
  ( ErrorLevel (..),
    QRImage (..),
  )
import Functora.Prelude

deriving stock instance Generic FileHeader

deriving stock instance Generic BitmapInfo

deriving stock instance Generic BitmapInfoV3

deriving stock instance Generic BitmapInfoV4

deriving stock instance Generic BitmapInfoV5

deriving stock instance Generic Compression

deriving stock instance Generic CIEXYZ

deriving stock instance Generic BMP

deriving stock instance Generic QRImage

deriving stock instance Generic ErrorLevel

instance NFData FileHeader

instance NFData BitmapInfo

instance NFData BitmapInfoV3

instance NFData BitmapInfoV4

instance NFData BitmapInfoV5

instance NFData Compression

instance NFData CIEXYZ

instance NFData BMP

instance NFData QRImage

instance NFData ErrorLevel
