module Functora.Miso.Widgets.Qr
  ( qr,
  )
where

import qualified Codec.QRCode as QRCode
import qualified Data.MemoUgly as Memo
import qualified Data.Text.Lazy as TL
import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Qr as Qr

qr :: MisoString -> [View action]
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
            . Memo.memo newQrImg
            $ fromMisoString txt
        ]

newQrImg :: MisoString -> Maybe MisoString
newQrImg =
  ( fmap
      $ toMisoString @TL.Text
      . Qr.qrToBmpDataUrlTL (Qr.Border 0) (Qr.Scale 5)
  )
    . QRCode.encodeAutomatic
      ( QRCode.defaultQRCodeOptions QRCode.L
      )
      QRCode.Iso8859_1OrUtf8WithoutECI
    . fromMisoString @TL.Text
