module App.Widgets.Qr
  ( qr,
    Opts (..),
    defOpts,
  )
where

import qualified App.Misc as Misc
import App.Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified Codec.QRCode as QRCode
import qualified Data.Function.Memoize as Memo
import qualified Data.Text.Lazy as TL
import qualified Functora.Prelude as Prelude
import qualified Functora.Qr as Qr
import qualified Material.Button as Button
import qualified Material.TextArea as TextArea
import Miso hiding (at, view)

qr :: Model -> Text -> Opts -> [View Action]
qr st txt opts
  | txt == mempty = mempty
  | otherwise =
      catMaybes
        [ fmap
            ( \img ->
                Cell.bigCell
                  $ img_
                    [ class_ "fill",
                      src_ img
                    ]
            )
            --
            -- TODO : Research how this works!
            -- It might be leaking memory or something..
            --
            . Memo.memoize newQrImg
            $ fromMisoString txt
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
                    & Button.setOnClick (Misc.copyIntoClipboardAction st txt)
                )
                "Copy"
          ]

newQrImg :: Prelude.String -> Maybe MisoString
newQrImg =
  ( fmap $ ms @TL.Text . Qr.qrToBmpDataUrlTL (Qr.Border 0) (Qr.Scale 5)
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
