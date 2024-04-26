module App.Widgets.TextProps
  ( textProps,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Switch as Switch
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.Theme as Theme
import Miso hiding (at, view)

textProps :: Model -> ATraversal' Model [TextProp Unique] -> [View Action]
textProps st optic = (<> [newButton optic]) $ do
  idx <- fst <$> zip [0 :: Int ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  textPropInputs st optic idx

textPropInputs ::
  Model ->
  ATraversal' Model [TextProp Unique] ->
  Int ->
  [View Action]
textPropInputs st optic idx =
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
    Switch.switch st ("Value " <> idxTxt <> " QR code")
      $ cloneTraversal optic
      . ix idx
      . #textPropValueQrCode,
    Button.smallButton
      [Theme.secondaryBg]
      ("Duplicate note " <> idxTxt)
      $ Misc.duplicateAt st optic idx,
    Button.smallButton
      [Theme.secondaryBg]
      ("Remove note " <> idxTxt)
      $ Misc.removeAt st optic idx
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)

newButton :: ATraversal' Model [TextProp Unique] -> View Action
newButton optic =
  Button.bigButton @Text mempty "Add new note" . PushUpdate $ do
    el <- newTextProp mempty mempty
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (<> [el]))
