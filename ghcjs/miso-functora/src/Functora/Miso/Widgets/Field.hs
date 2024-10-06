module Functora.Miso.Widgets.Field
  ( Args (..),
    Full (..),
    Opts (..),
    defOpts,
    OptsWidget (..),
    OptsWidgetPair (..),
    ModalWidget' (..),
    truncateUnicode,
    truncateDynamicField,
    expandDynamicField,
    field,
    ratioField,
    textField,
    dynamicField,
    passwordField,
    fieldViewer,
    fieldIcon,
  )
where

import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Widgets.Qr as Qr
import qualified Functora.Miso.Widgets.Select as Select
import qualified Language.Javascript.JSaddle as JS
import qualified Miso.String as MS

data Args model action t f = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Field t f),
    argsAction :: Update model -> action,
    argsEmitter :: Update model -> JSM ()
  }
  deriving stock (Generic)

data Full model action t f = Full
  { fullArgs :: Args model action t f,
    fullParser :: Field t f -> Maybe t,
    fullViewer :: t -> Unicode
  }
  deriving stock (Generic)

data Opts model action = Opts
  { optsLabel :: Maybe Unicode,
    optsDisabled :: Bool,
    optsFullWidth :: Bool,
    optsPlaceholder :: Unicode,
    optsOnInputAction :: Maybe (Update model -> action),
    optsLeadingWidget :: Maybe (OptsWidgetPair model action),
    optsTrailingWidget :: Maybe (OptsWidgetPair model action),
    optsOnKeyDownAction :: Uid -> KeyCode -> Update model,
    optsExtraAttributes :: [Attribute action]
  }
  deriving stock (Generic)

data OptsWidgetPair model action = OptsWidgetPair
  { optsWidgetPairEmpty :: OptsWidget model action,
    optsWidgetPairNonEmpty :: OptsWidget model action
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsLabel = Nothing,
      optsDisabled = False,
      optsFullWidth = False,
      optsPlaceholder = mempty,
      optsOnInputAction = Nothing,
      optsLeadingWidget = Just $ OptsWidgetPair PasteWidget PasteWidget,
      optsTrailingWidget = Just $ OptsWidgetPair ClearWidget ClearWidget,
      optsOnKeyDownAction = Jsm.enterOrEscapeBlur,
      optsExtraAttributes = mempty
    }

data OptsWidget model action
  = CopyWidget
  | ClearWidget
  | PasteWidget
  | ScanQrWidget
  | ShowOrHideWidget
  | ModalWidget (ModalWidget' model)
  | ActionWidget Unicode [Attribute action] action
  | forall item. UpWidget (ATraversal' model [item]) Int [Attribute action]
  | forall item. DownWidget (ATraversal' model [item]) Int [Attribute action]
  | forall item. DeleteWidget (ATraversal' model [item]) Int [Attribute action]

data ModalWidget' model where
  ModalItemWidget ::
    forall model a.
    ( Data a
    ) =>
    ATraversal' model [a] ->
    Int ->
    ATraversal' a [FieldPair DynamicField Unique] ->
    ATraversal' a (Field Unicode Unique) ->
    ATraversal' a OpenedOrClosed ->
    ModalWidget' model
  ModalFieldWidget ::
    forall model a b.
    ( Data a
    ) =>
    ATraversal' model [a] ->
    Int ->
    ATraversal' a (Field b Unique) ->
    StaticOrDynamic ->
    ModalWidget' model
  ModalMiniWidget ::
    forall model a.
    ( Data a
    ) =>
    ATraversal' model (Field a Unique) ->
    ModalWidget' model

field ::
  Full model action t Unique ->
  Opts model action ->
  [View action]
field Full {fullArgs = args, fullParser = parser, fullViewer = viewer} opts =
  ( do
      x0 <-
        catMaybes
          [ opts
              ^? #optsLeadingWidget
              . _Just
              . cloneTraversal widgetOptic,
            opts
              ^? #optsTrailingWidget
              . _Just
              . cloneTraversal widgetOptic
          ]
      x1 <-
        case x0 of
          ModalWidget w -> pure w
          _ -> mempty
      fieldModal args x1
  )
    <> [ maybe
          id
          (\x -> label_ mempty . (text x :) . singleton)
          (optsLabel opts)
          . input_
          $ ( catMaybes
                [ fmap
                    (type_ . htmlFieldType)
                    (st ^? cloneTraversal optic . #fieldType),
                  Just $ onInput onInputAction,
                  Just . disabled_ $ opts ^. #optsDisabled,
                  fmap placeholder_
                    $ if null placeholder
                      then optsLabel opts
                      else Just placeholder,
                  Just
                    . id_
                    . either impureThrow id
                    . decodeUtf8Strict
                    . unTagged
                    $ htmlUid uid,
                  Just . onKeyDown $ action . optsOnKeyDownAction opts uid,
                  Just $ onBlur onBlurAction
                ]
            )
          <> ( opts ^. #optsExtraAttributes
             )
       ]
  where
    --
    -- TODO : implement
    --
    -- & TextField.setLeadingIcon
    --   ( fmap
    --       (fieldIcon Leading full opts)
    --       (opts ^? #optsLeadingWidget . _Just . cloneTraversal widgetOptic)
    --   )
    -- & TextField.setTrailingIcon
    --   ( fmap
    --       (fieldIcon Trailing full opts)
    --       (opts ^? #optsTrailingWidget . _Just . cloneTraversal widgetOptic)
    --   )
    st = argsModel args
    optic = argsOptic args
    action = argsAction args
    placeholder = optsPlaceholder opts
    widgetOptic =
      if null . fromMaybe mempty $ getInput st
        then #optsWidgetPairEmpty
        else #optsWidgetPairNonEmpty
    uid =
      fromMaybe nilUid
        $ st
        ^? cloneTraversal optic
        . #fieldInput
        . #uniqueUid
    getInput st' =
      st' ^? cloneTraversal optic . #fieldInput . #uniqueValue
    getOutput st' =
      (st' ^? cloneTraversal optic >>= parser)
        <|> (st' ^? cloneTraversal optic . #fieldOutput)
    getInputReplacement st' = do
      let next = st' ^? cloneTraversal optic >>= parser
      inp <- getInput st'
      out <- getOutput st'
      if isJust next || (inp == viewer out)
        then Nothing
        else Just out
    onBlurAction =
      action . PureUpdate $ \prev ->
        prev
          & cloneTraversal optic
          . #fieldInput
          . #uniqueValue
          %~ maybe id (const . viewer) (getInputReplacement prev)
          & cloneTraversal optic
          . #fieldOutput
          %~ maybe id (const . id) (getOutput prev)
    onInputAction txt =
      fromMaybe action (optsOnInputAction opts) . PureUpdate $ \prev ->
        let next =
              prev
                & cloneTraversal optic
                . #fieldInput
                . #uniqueValue
                .~ txt
         in next
              & cloneTraversal optic
              . #fieldOutput
              %~ maybe id (const . id) (getOutput next)

ratioField ::
  Args model action Rational Unique ->
  Opts model action ->
  [View action]
ratioField args =
  field
    Full
      { fullArgs = args,
        fullParser = parseRatio . (^. #fieldInput . #uniqueValue),
        fullViewer = inspectRatioDef
      }

textField ::
  Args model action Unicode Unique ->
  Opts model action ->
  [View action]
textField args =
  field
    Full
      { fullArgs = args,
        fullParser = Just . (^. #fieldInput . #uniqueValue),
        fullViewer = id
      }

dynamicField ::
  Args model action DynamicField Unique ->
  Opts model action ->
  [View action]
dynamicField args =
  field
    Full
      { fullArgs = args,
        fullParser = parseDynamicField,
        fullViewer = inspectDynamicField
      }

passwordField ::
  Args model action Unicode Unique ->
  Opts model action ->
  [View action]
passwordField args opts =
  textField
    args
    ( opts
        & #optsPlaceholder
        .~ ("Password" :: Unicode)
        & #optsLeadingWidget
        .~ Just (OptsWidgetPair ShowOrHideWidget ShowOrHideWidget)
    )

fieldIcon ::
  Full model action t Unique ->
  Opts model action ->
  OptsWidget model action ->
  View action
fieldIcon full opts = \case
  CopyWidget ->
    fieldIconSimple "content_copy" mempty
      . action
      $ case st ^? cloneTraversal optic . #fieldInput . #uniqueValue of
        Nothing -> PureUpdate id
        Just txt -> Jsm.shareText txt
  ClearWidget ->
    fieldIconSimple "close" mempty
      . ( fromMaybe action
            $ optsOnInputAction opts
        )
      $ PureAndImpureUpdate
        ( cloneTraversal optic
            . #fieldInput
            . #uniqueValue
            .~ mempty
        )
        ( do
            focus
              . either impureThrow id
              . decodeUtf8Strict @Unicode
              . unTagged
              $ htmlUid uid
            void
              . JS.eval
              . either impureThrow id
              . decodeUtf8Strict @Unicode
              . unTagged
              $ "var el = document.getElementById('"
              <> htmlUid uid
              <> "'); if (el) el.value = '';"
            pure id
        )
  PasteWidget ->
    fieldIconSimple "content_paste_go" mempty
      $ insertAction full Jsm.selectClipboard
  ScanQrWidget ->
    fieldIconSimple "qr_code_scanner" mempty
      $ insertAction full Jsm.selectBarcode
  ShowOrHideWidget ->
    case st ^? cloneTraversal optic . #fieldType of
      Just FieldTypePassword ->
        fieldIconSimple "visibility_off" mempty
          . action
          . PureUpdate
          $ cloneTraversal optic
          . #fieldType
          .~ FieldTypeText
      _ ->
        fieldIconSimple "visibility" mempty
          . action
          . PureUpdate
          $ cloneTraversal optic
          . #fieldType
          .~ FieldTypePassword
  UpWidget opt idx attrs ->
    fieldIconSimple "keyboard_double_arrow_up" attrs
      . action
      $ Jsm.moveUp opt idx
  DownWidget opt idx attrs ->
    fieldIconSimple "keyboard_double_arrow_down" attrs
      . action
      $ Jsm.moveDown opt idx
  DeleteWidget opt idx attrs ->
    fieldIconSimple "delete_forever" attrs
      . action
      $ Jsm.removeAt opt idx
  ModalWidget (ModalItemWidget opt idx _ _ ooc) ->
    fieldIconSimple "settings" mempty
      . action
      . PureUpdate
      $ cloneTraversal opt
      . ix idx
      . cloneTraversal ooc
      .~ Opened
  ModalWidget (ModalFieldWidget opt idx access _) ->
    fieldIconSimple "settings" mempty
      . action
      . PureUpdate
      $ cloneTraversal opt
      . ix idx
      . cloneTraversal access
      . #fieldModalState
      .~ Opened
  ModalWidget (ModalMiniWidget opt) ->
    fieldIconSimple "settings" mempty
      . action
      . PureUpdate
      $ cloneTraversal opt
      . #fieldModalState
      .~ Opened
  ActionWidget icon attrs act ->
    fieldIconSimple icon attrs act
  where
    st = full ^. #fullArgs . #argsModel
    optic = full ^. #fullArgs . #argsOptic
    action = full ^. #fullArgs . #argsAction
    uid =
      fromMaybe nilUid
        $ st
        ^? cloneTraversal optic
        . #fieldInput
        . #uniqueUid

fieldIconSimple ::
  Unicode ->
  [Attribute action] ->
  action ->
  View action
fieldIconSimple txt attrs action =
  button_
    ( [ style_ [("pointer-events", "auto")],
        textProp "role" "button",
        intProp "tabindex" 0,
        onClick action
      ]
        <> attrs
    )
    [ text txt
    ]

fieldModal :: Args model action t f -> ModalWidget' model -> [View action]
fieldModal args@Args {argsAction = action} (ModalItemWidget opt idx fps lbl ooc) =
  Dialog.dialog
    Dialog.defOpts
    Dialog.Args
      { Dialog.argsModel = args ^. #argsModel,
        Dialog.argsOptic = cloneTraversal opt . ix idx . cloneTraversal ooc,
        Dialog.argsAction = args ^. #argsAction,
        Dialog.argsContent =
          [ Grid.mediumCell
              $ textField
                Args
                  { argsModel = args ^. #argsModel,
                    argsOptic = cloneTraversal opt . ix idx . cloneTraversal lbl,
                    argsAction = args ^. #argsAction,
                    argsEmitter = args ^. #argsEmitter
                  }
                ( defOpts
                    & #optsPlaceholder
                    .~ "Label"
                ),
            Grid.mediumCell
              [ button_
                  [ onClick
                      . action
                      . Jsm.addFieldPair
                      $ cloneTraversal opt
                      . ix idx
                      . cloneTraversal fps
                  ]
                  [text "Add note"]
              ],
            Grid.smallCell
              [ button_
                  [onClick . action $ Jsm.moveDown opt idx]
                  [text "Down"]
              ],
            Grid.smallCell
              [ button_
                  [onClick . action $ Jsm.moveUp opt idx]
                  [text "Up"]
              ],
            Grid.smallCell
              [ button_
                  [onClick . action $ Jsm.duplicateAt opt idx]
                  [text "Clone"]
              ],
            Grid.smallCell
              [ button_
                  [onClick . action $ Jsm.removeAt opt idx]
                  [text "Delete"]
              ]
          ]
      }
fieldModal args (ModalFieldWidget opt idx access sod) = do
  let st = args ^. #argsModel
  let optic =
        cloneTraversal opt
          . ix idx
          . cloneTraversal access
  let action =
        args ^. #argsAction
  Dialog.dialog
    Dialog.defOpts
    Dialog.Args
      { Dialog.argsModel = args ^. #argsModel,
        Dialog.argsOptic = cloneTraversal optic . #fieldModalState,
        Dialog.argsAction = args ^. #argsAction,
        Dialog.argsContent =
          ( case sod of
              Static -> mempty
              Dynamic ->
                singleton
                  $ Select.select
                    Select.defOpts
                    Select.Args
                      { Select.argsModel = st,
                        Select.argsOptic = cloneTraversal optic . #fieldType,
                        Select.argsAction = action,
                        Select.argsOptions = constTraversal $ enumerate @FieldType
                      }
          )
            <> [ -- Grid.mediumCell
                 --  $ Switch.switch
                 --    st
                 --    ( Switch.defOpts
                 --        & #optsIcon
                 --        .~ Just "content_copy"
                 --        & #optsPlaceholder
                 --        .~ "Allow copy"
                 --    )
                 --    ( cloneTraversal optic
                 --        . #fieldAllowCopy
                 --    ),
                 Grid.smallCell
                  [ button_
                      [onClick . action $ Jsm.moveDown opt idx]
                      [text "Down"]
                  ],
                 Grid.smallCell
                  [ button_
                      [onClick . action $ Jsm.moveUp opt idx]
                      [text "Up"]
                  ],
                 Grid.smallCell
                  [ button_
                      [onClick . action $ Jsm.duplicateAt opt idx]
                      [text "Clone"]
                  ],
                 Grid.smallCell
                  [ button_
                      [onClick . action $ Jsm.removeAt opt idx]
                      [text "Delete"]
                  ]
               ]
      }
fieldModal args (ModalMiniWidget opt) =
  Dialog.dialog
    Dialog.defOpts
    Dialog.Args
      { Dialog.argsModel = args ^. #argsModel,
        Dialog.argsOptic = cloneTraversal opt . #fieldModalState,
        Dialog.argsAction = args ^. #argsAction,
        Dialog.argsContent =
          [ Grid.bigCell
              [ selectTypeWidget args opt
              ]
          ]
      }

selectTypeWidget ::
  Args model action t f ->
  ATraversal' model (Field a Unique) ->
  View action
selectTypeWidget args@Args {argsAction = action} optic =
  Select.select
    Select.defOpts
    Select.Args
      { Select.argsModel = args ^. #argsModel,
        Select.argsOptic = cloneTraversal optic . #fieldType,
        Select.argsAction = action,
        Select.argsOptions = constTraversal $ enumerate @FieldType
      }

--
-- TODO : support optional copying widgets
--
fieldViewer ::
  ( Foldable1 f
  ) =>
  Args model action t f ->
  [View action]
fieldViewer args =
  case typ of
    FieldTypeNumber -> genericFieldViewer args text
    FieldTypePercent -> genericFieldViewer args $ text . (<> "%")
    FieldTypeText -> genericFieldViewer args text
    FieldTypeTitle -> header val
    FieldTypeHtml ->
      genericFieldViewer
        ( args
            & cloneTraversal optic
            . #fieldOpts
            . #fieldOptsTruncateLimit
            .~ Nothing
        )
        rawHtml
    FieldTypePassword ->
      genericFieldViewer args
        $ const "*****"
    FieldTypeQrCode ->
      genericFieldViewer
        ( args
            & cloneTraversal optic
            . #fieldOpts
            . #fieldOptsQrState
            .~ Just Opened
        )
        text
  where
    optic =
      #argsModel . argsOptic args
    typ =
      fromMaybe FieldTypeText $ args ^? cloneTraversal optic . #fieldType
    val =
      maybe mempty fold1 $ args ^? cloneTraversal optic . #fieldInput

header :: Unicode -> [View action]
header txt =
  if txt == mempty
    then mempty
    else
      [ div_
          [ style_ [("text-align", "center")]
          ]
          [ text txt
          ]
      ]

genericFieldViewer ::
  ( Foldable1 f
  ) =>
  Args model action t f ->
  (Unicode -> View action) ->
  [View action]
genericFieldViewer args widget =
  if input == mempty
    then mempty
    else
      ( case stateQr of
          Opened -> Qr.qr input
          Closed -> mempty
      )
        <> [ widget
              $ truncateFieldViewer
                allowTrunc
                stateTrunc
                (opts ^. #fieldOptsTruncateLimit)
                input
           ]
        <> ( if null extraWidgets
              then mempty
              else
                [ div_
                    [ style_ [("text-align", "right")]
                    ]
                    extraWidgets
                ]
           )
  where
    st = argsModel args
    optic = argsOptic args
    action = argsAction args
    input =
      maybe mempty fold1 $ st ^? cloneTraversal optic . #fieldInput
    opts =
      fromMaybe defFieldOpts $ st ^? cloneTraversal optic . #fieldOpts
    stateQr =
      fromMaybe Closed $ opts ^. #fieldOptsQrState
    allowCopy =
      opts ^. #fieldOptsAllowCopy
    allowTrunc =
      maybe False (length input >) $ opts ^. #fieldOptsTruncateLimit
    stateTrunc =
      fromMaybe Closed $ opts ^. #fieldOptsTruncateState
    extraWidgets =
      ( if not allowTrunc
          then mempty
          else do
            let icon = case stateTrunc of
                  Closed -> "open_in_full"
                  Opened -> "close_fullscreen"
            pure
              . fieldViewerIcon icon
              . action
              . PureUpdate
              $ cloneTraversal optic
              . #fieldOpts
              . #fieldOptsTruncateState
              . _Just
              %~ ( \case
                    Closed -> Opened
                    Opened -> Closed
                 )
      )
        <> ( if isNothing $ opts ^. #fieldOptsQrState
              then mempty
              else do
                let icon = case stateQr of
                      Closed -> "qr_code_2"
                      Opened -> "grid_off"
                pure
                  . fieldViewerIcon icon
                  . action
                  . PureUpdate
                  $ cloneTraversal optic
                  . #fieldOpts
                  . #fieldOptsQrState
                  . _Just
                  %~ ( \case
                        Closed -> Opened
                        Opened -> Closed
                     )
           )
        <> ( if not allowCopy
              then mempty
              else
                [ fieldViewerIcon "content_copy"
                    . action
                    $ Jsm.shareText input
                ]
           )

fieldViewerIcon :: Unicode -> action -> View action
fieldViewerIcon icon action =
  button_
    [onClick action]
    [text icon]

truncateFieldViewer ::
  Bool ->
  OpenedOrClosed ->
  Maybe Int ->
  Unicode ->
  Unicode
truncateFieldViewer True Closed limit full =
  truncateUnicode limit full
truncateFieldViewer _ _ _ full =
  full

truncateDynamicField ::
  Maybe Int ->
  Field DynamicField Identity ->
  Field DynamicField Identity
truncateDynamicField limit =
  (#fieldInput . #runIdentity %~ truncateUnicode limit)
    . ( #fieldOutput %~ \case
          DynamicFieldNumber {} -> DynamicFieldNumber 0
          DynamicFieldText {} -> DynamicFieldText mempty
      )

truncateUnicode :: Maybe Int -> Unicode -> Unicode
truncateUnicode limit input =
  if length input <= full
    then input
    else
      take half input
        <> "..."
        <> MS.takeEnd half input
  where
    full = fromMaybe defTruncateLimit limit
    half = full `div` 2

expandDynamicField ::
  Field DynamicField Identity ->
  Field DynamicField Identity
expandDynamicField x =
  if null inp
    then x & #fieldInput . #runIdentity .~ inspectDynamicField out
    else x & #fieldOutput .~ fromMaybe out (parseDynamicFieldId x)
  where
    inp = x ^. #fieldInput . #runIdentity
    out = x ^. #fieldOutput

insertAction ::
  Full model action t Unique ->
  ((Maybe Unicode -> JSM ()) -> JSM ()) ->
  action
insertAction Full {fullArgs = args, fullParser = parser} selector =
  action . ImpureUpdate $ do
    selector $ \case
      Nothing -> Jsm.popupText @Unicode "Failure!"
      Just inp -> do
        let next =
              prev
                & cloneTraversal optic
                . #fieldInput
                . #uniqueValue
                .~ inp
        case next ^? cloneTraversal optic >>= parser of
          Nothing -> Jsm.popupText @Unicode "Failure!"
          Just out ->
            emitter
              $ PureAndImpureUpdate
                ( \st ->
                    st
                      & cloneTraversal optic
                      . #fieldInput
                      . #uniqueValue
                      .~ inp
                      & cloneTraversal optic
                      . #fieldOutput
                      .~ out
                )
                ( do
                    Jsm.popupText @Unicode "Success!"
                    pure id
                )
    pure id
  where
    prev = args ^. #argsModel
    optic = args ^. #argsOptic
    action = args ^. #argsAction
    emitter = args ^. #argsEmitter
