module Functora.Miso.Widgets.FieldPairs
  ( Args (..),
    fieldPairsViewer,
    fieldPairsEditor,
  )
where

import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Typography as Typography

data Args model action f = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model [FieldPair DynamicField f],
    argsAction :: (model -> JSM model) -> action
  }
  deriving stock (Generic)

fieldPairsViewer :: (Foldable1 f) => Args model action f -> [View action]
fieldPairsViewer args@Args {argsOptic = optic} = do
  item <-
    zip [0 ..] . fromMaybe mempty $ args ^? #argsModel . cloneTraversal optic
  uncurry
    ( fieldPairViewer args
    )
    item

fieldPairViewer ::
  ( Foldable1 f
  ) =>
  Args model action f ->
  Int ->
  FieldPair DynamicField f ->
  [View action]
fieldPairViewer args@Args {argsOptic = optic} idx pair =
  ( if k == mempty
      then mempty
      else
        [ cell
            [ strong_
                [ Typography.typography,
                  Css.fullWidth,
                  class_ "mdc-text-field",
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
                [ text $ pair ^. #fieldPairKey . #fieldOutput
                ]
            ]
        ]
  )
    <> ( if v == mempty
          then mempty
          else
            [ cell
                $ Field.fieldViewer
                  Field.Args
                    { Field.argsModel =
                        args ^. #argsModel,
                      Field.argsOptic =
                        cloneTraversal optic . ix idx . #fieldPairValue,
                      Field.argsAction =
                        args ^. #argsAction
                    }
            ]
       )
  where
    k = pair ^. #fieldPairKey . #fieldOutput
    v = inspectDynamicField $ pair ^. #fieldPairValue . #fieldOutput
    cell =
      if k == mempty || v == mempty
        then Grid.bigCell
        else Grid.mediumCell

fieldPairsEditor :: Args model action Unique -> [View action]
fieldPairsEditor args@Args {argsModel = st, argsOptic = optic} = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  fieldPairEditor args idx

fieldPairEditor ::
  forall model action.
  Args model action Unique ->
  Int ->
  [View action]
fieldPairEditor Args {argsModel = st, argsOptic = optic, argsAction = action} idx =
  [ Field.textField
      Field.Args
        { Field.argsModel = st,
          Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairKey,
          Field.argsAction = action
        }
      ( Field.defOpts @model @action
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
          & ( #optsLeadingWidget ::
                Lens'
                  (Field.Opts model action)
                  (Maybe (Field.OptsWidget model action))
            )
          .~ Just (Field.DownWidget optic idx mempty)
          & #optsTrailingWidget
          .~ Just (Field.UpWidget optic idx mempty)
      ),
    Field.dynamicField
      Field.Args
        { Field.argsModel = st,
          Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairValue,
          Field.argsAction = action
        }
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
          & ( #optsLeadingWidget ::
                Lens'
                  (Field.Opts model action)
                  (Maybe (Field.OptsWidget model action))
            )
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
