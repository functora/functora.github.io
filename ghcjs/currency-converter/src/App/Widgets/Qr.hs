module App.Widgets.Qr
  ( qr,
    Opts (..),
    defOpts,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Cell as Cell
import qualified Codec.QRCode as QRCode
import Functora.Prelude hiding (Field)
import qualified Functora.Qr as Qr
import Miso hiding (at, view)
import Miso.String (ms)

qr :: Model -> Text -> Opts -> [View Action]
qr st txt opts
  | null txt = mempty
  | otherwise =
      catMaybes
        [ fmap
            ( \img ->
                Cell.bigCell
                  [ img_
                      [ class_ "fill",
                        src_ $ ms img
                      ]
                  ]
            )
            $ newQrImg txt
        ]
        <> copyWidget
        <> fmap (\x -> extraCell [x]) extraWidgets
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
              [ textarea_
                  [ value_ $ ms txt,
                    disabled_ True
                  ]
                  mempty
              ],
            extraCell
              [ Button.button
                  ( Button.defOpts
                      & #optsLabel
                      .~ Just "Copy"
                      & ( #optsOnClick :: Lens' (Button.Opts Action) (Maybe Action)
                        )
                      .~ Just (Misc.copyIntoClipboardAction st txt)
                      & ( #optsLeadingIcon :: Lens' (Button.Opts Action) (Maybe Text)
                        )
                      .~ Just "content_copy"
                  )
              ]
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
