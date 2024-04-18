module App.Widgets.TextProps
  ( textProps,
  )
where

import App.Types
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
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
  [ TextInput.textInput
      st
      "Key"
      ( cloneLens optic
          . ix idx
          . #textPropKey
      ),
    TextInput.textInput
      st
      "Value"
      ( cloneLens optic
          . ix idx
          . #textPropValue
      )
  ]
