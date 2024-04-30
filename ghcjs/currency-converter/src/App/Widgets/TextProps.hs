module App.Widgets.TextProps
  ( textProps,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Header as Header
import qualified App.Widgets.IconToggles as IconToggles
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.IconToggle as IconToggle
import Miso hiding (at, view)

textProps ::
  HeaderOrFooter -> Model -> ATraversal' Model [TextProp Unique] -> [View Action]
textProps hof st optic =
  case hof of
    Header -> (Header.header "Details" (Just (Misc.newTextPropAction optic)) :)
    Footer -> id
    $ do
      idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
      textPropWidget st optic idx

textPropWidget ::
  Model ->
  ATraversal' Model [TextProp Unique] ->
  Int ->
  [View Action]
textPropWidget st optic idx =
  [ TextInput.textInput
      st
      ( cloneTraversal optic
          . ix idx
          . #textPropKey
      )
      ( TextInput.opts
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
      ),
    TextInput.textInput
      st
      ( cloneTraversal optic
          . ix idx
          . #textPropValue
      )
      ( TextInput.opts
          & #optsPlaceholder
          .~ ("Value " <> idxTxt)
      ),
    IconToggles.iconToggles
      st
      [ ( "Value " <> idxTxt <> " text",
          IconToggle.icon "font_download",
          IconToggle.icon "font_download_off",
          cloneTraversal optic . ix idx . #textPropValuePlainText
        ),
        ( "Value " <> idxTxt <> " QR code",
          IconToggle.icon "qr_code_2",
          IconToggle.icon "developer_board_off",
          cloneTraversal optic . ix idx . #textPropValueQrCode
        ),
        ( "Value " <> idxTxt <> " link",
          IconToggle.icon "link",
          IconToggle.icon "link_off",
          cloneTraversal optic . ix idx . #textPropValueLink
        ),
        ( "Value " <> idxTxt <> " HTML",
          IconToggle.icon "code",
          IconToggle.icon "code_off",
          cloneTraversal optic . ix idx . #textPropValueHtml
        )
      ],
    Header.navHeaderSimple st optic idx
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
