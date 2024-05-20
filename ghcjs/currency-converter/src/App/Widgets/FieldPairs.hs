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
  [ Field.textField
      st
      ( cloneTraversal optic
          . ix idx
          . #fieldPairKey
      )
      ( Field.defOpts
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
          & #optsLeadingWidget
          .~ Just (Field.DownWidget optic idx mempty)
          & #optsTrailingWidget
          .~ Just (Field.UpWidget optic idx mempty)
      ),
    Field.dynamicField
      st
      optic
      idx
      ( Field.defOpts
          & #optsPlaceholder
          .~ ( "Value "
                <> idxTxt
                <> ( maybe mempty (" - " <>)
                      $ st
                      ^? cloneTraversal optic
                      . ix idx
                      . #fieldPairValue
                      . #fieldType
                      . to userFieldType
                   )
             )
          & #optsLeadingWidget
          .~ Just
            ( Field.ModalWidget
                $ Field.ModalFieldWidget
                  optic
                  idx
                  #fieldPairValue
                  Dynamic
            )
          & #optsTrailingWidget
          .~ Just
            ( Field.DeleteWidget optic idx mempty
            )
      )
  ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
