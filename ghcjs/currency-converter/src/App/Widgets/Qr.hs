module App.Widgets.Qr
  ( qr,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified Codec.QRCode as QRCode
import Functora.Prelude hiding (Field)
import qualified Functora.Qr as Qr
import Miso hiding (at, view)
import Miso.String (ms)

qr :: Maybe Text -> Field.Opts -> [View Action]
qr mTxt _ =
  catMaybes
    $ [ fmap
          ( \img ->
              Cell.bigCell
                $ img_
                  [ class_ "fill",
                    src_ $ ms img
                  ]
          )
          mImg
      ]
  where
    mImg = mTxt >>= newQrImg

newQrImg :: Text -> Maybe Text
newQrImg =
  ( fmap $ Qr.qrToBmpDataUrlTL (Qr.Border 0) (Qr.Scale 5)
  )
    . QRCode.encodeAutomatic
      ( QRCode.defaultQRCodeOptions QRCode.L
      )
      QRCode.Iso8859_1OrUtf8WithoutECI
