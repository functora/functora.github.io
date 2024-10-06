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

data Args model action f = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model [FieldPair DynamicField f],
    argsAction :: Update model -> action,
    argsEmitter :: Update model -> JSM ()
  }
  deriving stock (Generic)

newtype Opts = Opts
  { optsAdvanced :: Bool
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsAdvanced = True
    }

fieldPairsViewer :: (Foldable1 f) => Args model action f -> [View action]
fieldPairsViewer args@Args {argsOptic = optic} =
  if null content
    then mempty
    else [dl_ mempty content]
  where
    content = do
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
        [ dt_
            mempty
            [ text $ pair ^. #fieldPairKey . #fieldOutput
            ]
        ]
  )
    <> ( if v == mempty
          then mempty
          else
            singleton
              . dd_ mempty
              $ Field.fieldViewer
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
    k = pair ^. #fieldPairKey . #fieldOutput
    v = inspectDynamicField $ pair ^. #fieldPairValue . #fieldOutput

fieldPairsEditor :: Args model action Unique -> Opts -> [View action]
fieldPairsEditor args@Args {argsModel = st, argsOptic = optic} opts = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  fieldPairEditor args opts idx

fieldPairEditor ::
  forall model action.
  Args model action Unique ->
  Opts ->
  Int ->
  [View action]
fieldPairEditor
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter
    }
  Opts
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
      ( Field.defOpts
          & #optsLabel
          .~ Just
            ( fromMaybe ("#" <> inspect (idx + 1))
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
  Opts
    { optsAdvanced = True
    }
  idx =
    Field.textField
      Field.Args
        { Field.argsModel = st,
          Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairKey,
          Field.argsAction = action,
          Field.argsEmitter = emitter
        }
      ( Field.defOpts @model @action
          & #optsPlaceholder
          .~ ("Label " <> idxTxt)
          & ( #optsLeadingWidget ::
                Lens'
                  (Field.Opts model action)
                  (Maybe (Field.OptsWidgetPair model action))
            )
          .~ Just
            ( let w = Field.DownWidget optic idx mempty
               in Field.OptsWidgetPair w w
            )
          & #optsTrailingWidget
          .~ Just
            ( let w = Field.UpWidget optic idx mempty
               in Field.OptsWidgetPair w w
            )
      )
      <> Field.dynamicField
        Field.Args
          { Field.argsModel = st,
            Field.argsOptic = cloneTraversal optic . ix idx . #fieldPairValue,
            Field.argsAction = action,
            Field.argsEmitter = emitter
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
                    (Maybe (Field.OptsWidgetPair model action))
              )
            .~ Just
              ( let w =
                      Field.ModalWidget
                        $ Field.ModalFieldWidget
                          optic
                          idx
                          #fieldPairValue
                          Dynamic
                 in Field.OptsWidgetPair w w
              )
            & #optsTrailingWidget
            .~ Just
              ( let w = Field.DeleteWidget optic idx mempty
                 in Field.OptsWidgetPair w w
              )
        )
    where
      idxTxt :: Unicode
      idxTxt = "#" <> inspect (idx + 1)
