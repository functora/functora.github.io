module App.Widgets.TextProps
  ( textProps,
  )
where

import App.Types
import qualified App.Widgets.Switch as Switch
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import Miso hiding (at, view)

textProps :: Model -> ALens' Model [TextProp Unique] -> [View Action]
textProps st optic =
  zip [0 :: Int ..] (st ^. cloneLens optic)
    >>= textPropInputs st optic
    . fst

textPropInputs ::
  Model ->
  ALens' Model [TextProp Unique] ->
  Int ->
  [View Action]
textPropInputs st optic idx =
  [ TextInput.textInput st ("Label " <> idxTxt)
      $ cloneLens optic
      . ix idx
      . #textPropKey,
    TextInput.textInput st ("Value " <> idxTxt)
      $ cloneLens optic
      . ix idx
      . #textPropValue,
    Switch.switch st ("Value " <> idxTxt <> " QR code")
      $ cloneLens optic
      . ix idx
      . #textPropValueQrCode,
    textPropButton ("Duplicate " <> idxTxt) duplicateValue,
    textPropButton ("Remove " <> idxTxt) removeValue
  ]
  where
    idxTxt = "#" <> inspect (idx + 1)

textPropButton ::
  forall a action.
  ( From a String
  ) =>
  a ->
  action ->
  View action
textPropButton label action =
  LayoutGrid.cell
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span2Phone
    ]
    [ Button.raised
        ( Button.setOnClick action
            . Button.setAttributes
              [ class_ "fill",
                Theme.secondaryBg
              ]
            $ Button.config
        )
        ( from @a @String label
        )
    ]

duplicateValue :: Action
duplicateValue =
  Noop

removeValue :: Action
removeValue =
  Noop
