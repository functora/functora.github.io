module App.Widgets.TextProps
  ( textProps,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Switch as Switch
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
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
    smallButton ("Duplicate note " <> idxTxt) $ Misc.duplicateAt st optic idx,
    smallButton ("Remove note " <> idxTxt) $ Misc.removeAt st optic idx
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)

newButton :: ATraversal' Model [TextProp Unique] -> View Action
newButton optic =
  bigButton @Text "Add new note" . PushUpdate $ do
    el <- newTextProp mempty mempty
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (<> [el]))

bigButton :: forall a action. (From a String) => a -> action -> View action
bigButton =
  button @a @action
    [ LayoutGrid.span12
    ]

smallButton :: forall a action. (From a String) => a -> action -> View action
smallButton =
  button @a @action
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span2Phone
    ]

button ::
  forall a action.
  ( From a String
  ) =>
  [Attribute action] ->
  a ->
  action ->
  View action
button attrs label action =
  LayoutGrid.cell
    attrs
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
