module Functora.Miso.Widgets.FieldPairs
  ( Args (..),
    Opts (..),
    defOpts,
    fieldPairsViewer,
    fieldPairsEditor,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.Flex as Flex
import qualified Functora.Miso.Widgets.Icon as Icon

data Args model action f = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model [FieldPair DynamicField f],
    argsAction :: Update model -> action,
    argsEmitter :: Update model -> JSM ()
  }
  deriving stock (Generic)

data Opts model action f = Opts
  { optsIcon :: Icon.Icon -> View action,
    optsField :: Int -> Field.Opts model action DynamicField f,
    optsAdvanced :: Bool
  }
  deriving stock (Generic)

defOpts :: Opts model action f
defOpts =
  Opts
    { optsIcon = Icon.icon @Icon.Fa,
      optsField = const Field.defOpts,
      optsAdvanced = True
    }

fieldPairsViewer ::
  ( Foldable1 f
  ) =>
  Opts model action f ->
  Args model action f ->
  [View action]
fieldPairsViewer opts args@Args {argsOptic = optic} =
  if null content
    then mempty
    else [dl_ mempty content]
  where
    content = do
      item <-
        zip [0 ..] . fromMaybe mempty $ args ^? #argsModel . cloneTraversal optic
      uncurry
        ( fieldPairViewer opts args
        )
        item

fieldPairViewer ::
  forall model action f.
  ( Foldable1 f
  ) =>
  Opts model action f ->
  Args model action f ->
  Int ->
  FieldPair DynamicField f ->
  [View action]
fieldPairViewer opts args@Args {argsOptic = optic} idx pair =
  ( if k == mempty
      then mempty
      else [dt_ mempty [text k]]
  )
    <> ( if v == mempty
          then mempty
          else
            Field.fieldViewer
              ( optsField opts idx
                  & #optsIcon
                  .~ optsIcon opts
                  & #optsLeftRightViewer
                  .~ Flex.flexLeftRight dd_ id
              )
              Field.Args
                { Field.argsModel =
                    args ^. #argsModel,
                  Field.argsOptic =
                    cloneTraversal optic . ix idx . #fieldPairValue,
                  Field.argsAction =
                    args ^. #argsAction,
                  Field.argsEmitter =
                    args ^. #argsEmitter
                }
       )
  where
    k = inspectDynamicField $ pair ^. #fieldPairKey . #fieldOutput
    v = inspectDynamicField $ pair ^. #fieldPairValue . #fieldOutput

fieldPairsEditor ::
  Args model action Unique ->
  Opts model action Unique ->
  [View action]
fieldPairsEditor args@Args {argsModel = st, argsOptic = optic} opts = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  fieldPairEditor args opts idx

fieldPairEditor ::
  forall model action.
  Args model action Unique ->
  Opts model action Unique ->
  Int ->
  [View action]
fieldPairEditor
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter
    }
  opts@Opts
    { optsAdvanced = False
    }
  idx =
    Field.dynamicField
      Field.Args
        { Field.argsModel = st,
          Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairValue,
          Field.argsAction = action,
          Field.argsEmitter = emitter
        }
      ( optsField opts idx
          & #optsLabel
          .~ Just
            ( maybe ("#" <> inspect (idx + 1)) inspectDynamicField
                $ st
                ^? cloneTraversal optic
                . ix idx
                . #fieldPairKey
                . #fieldOutput
            )
      )
fieldPairEditor
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter
    }
  opts@Opts
    { optsAdvanced = True
    }
  idx =
    Field.dynamicField
      Field.Args
        { Field.argsModel = st,
          Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairKey,
          Field.argsAction = action,
          Field.argsEmitter = emitter
        }
      ( optsField opts idx
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
          & #optsTrailingWidgets
          .~ ( \_ _ _ ->
                [ Field.DownWidget optic idx mempty,
                  Field.UpWidget optic idx mempty
                ]
             )
      )
      <> Field.dynamicField
        Field.Args
          { Field.argsModel = st,
            Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairValue,
            Field.argsAction = action,
            Field.argsEmitter = emitter
          }
        ( optsField opts idx
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
            & #optsTrailingWidgets
            .~ ( \_ _ _ ->
                  [ Field.ModalWidget
                      $ Field.ModalFieldWidget
                        optic
                        idx
                        #fieldPairValue
                        Dynamic,
                    Field.DeleteWidget optic idx mempty
                  ]
               )
        )
    where
      idxTxt :: Unicode
      idxTxt = "#" <> inspect (idx + 1)
