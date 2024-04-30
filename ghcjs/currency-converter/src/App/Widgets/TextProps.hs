module App.Widgets.TextProps
  ( textProps,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Chips as Chips
import qualified App.Widgets.Header as Header
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.Chip.Filter as Filter
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
    Chips.chips
      st
      [ ( "Text",
          Just $ Filter.icon "title",
          cloneTraversal optic . ix idx . #textPropValuePlainText
        ),
        ( "QR",
          Just $ Filter.icon "qr_code_2",
          cloneTraversal optic . ix idx . #textPropValueQrCode
        ),
        ( "Link",
          Just $ Filter.icon "link",
          cloneTraversal optic . ix idx . #textPropValueLink
        ),
        ( "HTML",
          Just $ Filter.icon "code",
          cloneTraversal optic . ix idx . #textPropValueHtml
        )
      ],
    Header.navHeaderSimple st optic idx
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
