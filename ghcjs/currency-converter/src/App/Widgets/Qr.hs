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

qr :: Model -> Text -> Field.Opts -> [View Action]
qr st txt opts =
  catMaybes
    $ [ fmap
          ( \img ->
              Cell.bigCell
                $ img_
                  [ class_ "fill",
                    src_ $ ms img
                  ]
          )
          $ newQrImg txt,
        Just
          . Cell.bigCell
          $ Field.constTextField st txt opts
      ]

newQrImg :: Text -> Maybe Text
newQrImg =
  ( fmap $ Qr.qrToBmpDataUrlTL (Qr.Border 0) (Qr.Scale 5)
  )
    . QRCode.encodeAutomatic
      ( QRCode.defaultQRCodeOptions QRCode.L
      )
      QRCode.Iso8859_1OrUtf8WithoutECI
