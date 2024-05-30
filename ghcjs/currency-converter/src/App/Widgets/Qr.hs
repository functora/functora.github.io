module App.Widgets.Qr
  ( qr,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified Codec.QRCode as QRCode
import Functora.Prelude hiding (Field)
import qualified Functora.Qr as Qr
import qualified Material.Button as Button
import qualified Material.TextArea as TextArea
import Miso hiding (at, view)
import Miso.String (ms)

qr :: Model -> Text -> Bool -> [View Action]
qr st txt allowCopy =
  catMaybes
    [ fmap
        ( \img ->
            Cell.bigCell
              $ img_
                [ class_ "fill",
                  src_ $ ms img
                ]
        )
        $ newQrImg txt
    ]
    <> copyWidget
  where
    copyWidget =
      if not allowCopy
        then mempty
        else
          [ Cell.bigCell
              . TextArea.filled
              $ TextArea.config
              & TextArea.setValue (Just $ from @Text @String txt)
              & TextArea.setDisabled True
              & TextArea.setFullwidth True,
            Cell.bigCell
              $ Button.raised
                ( Button.config
                    & Button.setIcon (Just "content_copy")
                    & Button.setAttributes [class_ "fill"]
                    & Button.setOnClick
                      ( PushUpdate $ do
                          Misc.copyIntoClipboard st txt
                          pure $ ChanItem 0 id
                      )
                )
                "Copy"
          ]

newQrImg :: Text -> Maybe Text
newQrImg =
  ( fmap $ Qr.qrToBmpDataUrlTL (Qr.Border 0) (Qr.Scale 5)
  )
    . QRCode.encodeAutomatic
      ( QRCode.defaultQRCodeOptions QRCode.L
      )
      QRCode.Iso8859_1OrUtf8WithoutECI
