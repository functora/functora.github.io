module App.Widgets.FieldPairs
  ( fieldPairsViewer,
    fieldPairs,
  )
where

import App.Types
import qualified App.Widgets.Field as Field
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Typography as Typography

fieldPairsViewer :: Model -> [FieldPair DynamicField Unique] -> [View Action]
fieldPairsViewer st = (>>= fieldPairViewer st)

fieldPairViewer :: Model -> FieldPair DynamicField Unique -> [View Action]
fieldPairViewer st pair =
  ( if k == mempty
      then mempty
      else
        [ cell
            $ strong_
              [ Typography.typography,
                class_ "fill",
                class_ "mdc-text-field",
                class_ "mdc-text-field--filled",
                style_
                  [ ("align-items", "center"),
                    ("align-content", "center"),
                    ("word-break", "normal"),
                    ("overflow-wrap", "anywhere"),
                    ("min-height", "56px"),
                    ("height", "auto"),
                    ("padding-top", "8px"),
                    ("padding-bottom", "8px"),
                    ("line-height", "150%")
                  ]
              ]
              [text $ pair ^. #fieldPairKey . #fieldOutput]
        ]
  )
    <> ( if v == mempty
          then mempty
          else
            [ cell
                . div_ mempty
                $ Field.dynamicFieldViewer st (pair ^. #fieldPairValue)
            ]
       )
  where
    k = pair ^. #fieldPairKey . #fieldOutput
    v = inspectDynamicField $ pair ^. #fieldPairValue . #fieldOutput
    cell =
      if k == mempty || v == mempty
        then Grid.bigCell
        else Grid.mediumCell

fieldPairs ::
  Model ->
  ATraversal' Model [FieldPair DynamicField Unique] ->
  [View Action]
fieldPairs st optic = do
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
    idxTxt :: MisoString
    idxTxt = "#" <> inspect (idx + 1)
