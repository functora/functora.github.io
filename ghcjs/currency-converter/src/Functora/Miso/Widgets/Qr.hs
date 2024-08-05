module Functora.Miso.Widgets.Qr
  ( qr,
    Opts (..),
    defOpts,
    newQrImg,
  )
where

import qualified Codec.QRCode as QRCode
import qualified Data.MemoUgly as Memo
import qualified Data.Text.Lazy as TL
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Qr as Qr
import qualified Material.Button as Button
import qualified Material.TextArea as TextArea

qr :: model -> MisoString -> Opts model action -> [View action]
qr st txt opts
  | txt == mempty = mempty
  | otherwise =
      catMaybes
        [ fmap
            ( \img ->
                Grid.bigCell
                  $ img_
                    [ class_ "fill",
                      src_ img
                    ]
            )
            --
            -- TODO : Research how this works!
            -- It might be leaking memory or something..
            --
            . Memo.memo newQrImg
            $ fromMisoString txt
        ]
        <> copyWidget
        <> fmap extraCell extraWidgets
  where
    allowCopy = opts ^. #optsAllowCopy
    extraWidgets = opts ^. #optsExtraWidgets
    extraCell =
      case mod (length extraWidgets + if allowCopy then 1 else 0) 2 of
        0 -> Grid.mediumCell
        _ -> Grid.bigCell
    copyWidget =
      if not $ opts ^. #optsAllowCopy
        then mempty
        else
          [ Grid.bigCell
              . TextArea.filled
              $ TextArea.config
              & TextArea.setValue (Just txt)
              & TextArea.setDisabled True
              & TextArea.setFullwidth True
              & TextArea.setInnerView [text txt],
            extraCell
              $ Button.raised
                ( Button.config
                    & Button.setIcon (Just "share")
                    & Button.setAttributes [class_ "fill"]
                    & ( maybe id (\f -> Button.setOnClick $ f st txt)
                          $ optsCopyIntoClipboardAction opts
                      )
                )
                "Copy"
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

data Opts model action = Opts
  { optsAllowCopy :: Bool,
    optsExtraWidgets :: [View action],
    optsCopyIntoClipboardAction :: Maybe (model -> MisoString -> action)
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsAllowCopy = True,
      optsExtraWidgets = mempty,
      optsCopyIntoClipboardAction = Nothing
    }
