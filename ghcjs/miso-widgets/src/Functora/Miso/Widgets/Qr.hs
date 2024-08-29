module Functora.Miso.Widgets.Qr
  ( Args (..),
    Opts (..),
    defOpts,
    qr,
  )
where

import qualified Codec.QRCode as QRCode
import qualified Data.MemoUgly as Memo
import qualified Data.Text.Lazy as TL
import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Qr as Qr

data Args model action = Args
  { argsValue :: MisoString,
    argsAction :: (model -> JSM model) -> action
  }
  deriving stock (Generic)

data Opts action = Opts
  { optsAllowCopy :: Bool,
    optsExtraWidgets :: [View action]
  }
  deriving stock (Generic)

defOpts :: Opts action
defOpts =
  Opts
    { optsAllowCopy = True,
      optsExtraWidgets = mempty
    }

qr :: Args model action -> Opts action -> [View action]
qr args opts
  | txt == mempty = mempty
  | otherwise =
      catMaybes
        [ fmap
            ( \img ->
                Grid.bigCell
                  $ img_
                    [ Css.fullWidth,
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
    txt = args ^. #argsValue
    action = args ^. #argsAction
    -- allowCopy = opts ^. #optsAllowCopy
    extraWidgets = opts ^. #optsExtraWidgets
    extraCell =
      case mod (length extraWidgets) 2 of
        0 -> Grid.mediumCell
        _ -> Grid.bigCell
    copyWidget =
      if not $ opts ^. #optsAllowCopy
        then mempty
        else
          [ Grid.bigCell
              . div_ mempty
              $ genericFieldViewer
                action
                (newFieldId FieldTypeText id txt)
                text
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
