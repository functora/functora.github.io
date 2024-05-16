module App.Widgets.FieldPairs
  ( fieldPairs,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import Functora.Prelude as Prelude
import Miso hiding (at, view)

fieldPairs ::
  HeaderOrFooter ->
  Model ->
  ATraversal' Model [FieldPair DynamicField Unique] ->
  [View Action]
fieldPairs hof st optic =
  case hof of
    Header -> (Header.header "Details" (Just (Misc.newFieldPairAction optic)) :)
    Footer -> id
    $ do
      idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
      fieldPairWidget st optic idx

fieldPairWidget ::
  Model ->
  ATraversal' Model [FieldPair DynamicField Unique] ->
  Int ->
  [View Action]
fieldPairWidget st optic idx =
  [ Field.textField @Text
      st
      ( Left
          $ cloneTraversal optic
          . ix idx
          . #fieldPairKey
      )
      ( Field.defOpts
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
      ),
    Field.dynamicField
      st
      optic
      idx
      ( Field.defOpts
          & #optsPlaceholder
          .~ ("Value " <> idxTxt)
      )
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
