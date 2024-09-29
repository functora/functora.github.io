module Functora.Miso.Capa.Qr
  ( qr,
  )
where

import qualified Codec.QRCode as QRCode
import qualified Data.ByteString.Lazy as BL
import qualified Data.MemoUgly as Memo
import qualified Functora.Miso.Capa.Grid as Grid
import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import qualified Functora.Qr as Qr

qr :: Unicode -> [View action]
qr txt
  | txt == mempty = mempty
  | otherwise =
      catMaybes
        [ fmap
            ( \img ->
                Grid.bigCell
                  [ img_
                      [ Css.fullWidth,
                        src_ img
                      ]
                  ]
            )
            --
            -- TODO : Research how this works!
            -- It might be leaking memory or something..
            --
            $ Memo.memo newQrImg txt
        ]

newQrImg :: Unicode -> Maybe Unicode
newQrImg =
  ( rightToMaybe
      . decodeUtf8Strict @Unicode @BL.ByteString
      . Qr.qrToBmpDataUrlBL (Qr.Border 0) (Qr.Scale 5)
  )
    <=< QRCode.encodeAutomatic
      ( QRCode.defaultQRCodeOptions QRCode.L
      )
      QRCode.Iso8859_1OrUtf8WithoutECI
    . from @Unicode @String
