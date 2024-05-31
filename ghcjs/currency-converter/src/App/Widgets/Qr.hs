module App.Widgets.Qr
  ( qr,
    Opts (..),
    defOpts,
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

qr :: Model -> Text -> Opts -> [View Action]
qr st txt opts =
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
    <> fmap extraCell extraWidgets
  where
    allowCopy = opts ^. #optsAllowCopy
    extraWidgets = opts ^. #optsExtraWidgets
    extraCell =
      case mod (length extraWidgets + if allowCopy then 1 else 0) 2 of
        0 -> Cell.mediumCell
        _ -> Cell.bigCell
    copyWidget =
      if not $ opts ^. #optsAllowCopy
        then mempty
        else
          [ Cell.bigCell
              . TextArea.filled
              $ TextArea.config
              & TextArea.setValue (Just $ from @Text @String txt)
              & TextArea.setDisabled True
              & TextArea.setFullwidth True,
            extraCell
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

data Opts = Opts
  { optsAllowCopy :: Bool,
    optsExtraWidgets :: [View Action]
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsAllowCopy = True,
      optsExtraWidgets = mempty
    }
