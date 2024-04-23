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
textProps st optic =
  zip [0 :: Int ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
    >>= textPropInputs st optic
    . fst

textPropInputs ::
  Model ->
  ATraversal' Model [TextProp Unique] ->
  Int ->
  [View Action]
textPropInputs st optic idx =
  [ TextInput.textInput st ("Label " <> idxTxt)
      $ cloneTraversal optic
      . ix idx
      . #textPropKey,
    TextInput.textInput st ("Value " <> idxTxt)
      $ cloneTraversal optic
      . ix idx
      . #textPropValue,
    Switch.switch st ("Value " <> idxTxt <> " QR code")
      $ cloneTraversal optic
      . ix idx
      . #textPropValueQrCode,
    textPropButton ("Duplicate " <> idxTxt) $ duplicateValue st optic idx,
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

duplicateValue ::
  Model ->
  ATraversal' Model [TextProp Unique] ->
  Int ->
  Action
duplicateValue st optic idx =
  PushUpdate $ do
    duplicator <- newUniqueDuplicator @Text
    let updater loc el =
          if loc == idx
            then [el, duplicator el]
            else [el]
    --
    -- TODO : maybe move to general update function?
    -- Probably overhead is not to big,
    -- and this will cover all corner cases everywhere.
    --
    Misc.forceRender st
    pure
      . ChanItem 0
      $ (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

removeValue :: Action
removeValue =
  Noop
