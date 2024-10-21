module Functora.Miso.Widgets.Field
  ( Args (..),
    Full (..),
    Opts (..),
    defOpts,
    OptsWidget (..),
    OptsWidgetPair (..),
    ModalWidget' (..),
    truncateUnicode,
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
import qualified Functora.Miso.Widgets.Icon as Icon
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
  { optsIcon :: Icon.Icon -> View action,
    optsLabel :: Maybe Unicode,
    optsDisabled :: Bool,
    optsFullWidth :: Bool,
    optsPlaceholder :: Unicode,
    optsOnInputAction :: Maybe (Update model -> action),
    optsLeadingWidget :: Maybe (OptsWidgetPair model action),
    optsTrailingWidget :: Maybe (OptsWidgetPair model action),
    optsOnKeyDownAction :: Uid -> KeyCode -> Update model,
    optsExtraAttributes :: [Attribute action],
    optsLeftRightViewer :: [View action] -> [View action] -> [View action]
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
    { optsIcon = Icon.icon @Icon.Fa,
      optsLabel = Nothing,
      optsDisabled = False,
      optsFullWidth = False,
      optsPlaceholder = mempty,
      optsOnInputAction = Nothing,
      optsLeadingWidget = Just $ OptsWidgetPair PasteWidget PasteWidget,
      optsTrailingWidget = Just $ OptsWidgetPair ClearWidget ClearWidget,
      optsOnKeyDownAction = Jsm.enterOrEscapeBlur,
      optsExtraAttributes = mempty,
      optsLeftRightViewer = (<>)
    }

data OptsWidget model action
  = CopyWidget
  | ClearWidget
  | PasteWidget
  | ScanQrWidget
  | ShowOrHideWidget
  | ModalWidget (ModalWidget' model)
  | ActionWidget Icon.Icon [Attribute action] action
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
field full@Full {fullArgs = args, fullParser = parser, fullViewer = viewer} opts =
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
    <> maybe
      id
      ( \x ->
          singleton
            . label_ mempty
            . (text x :)
            . (br_ mempty :)
      )
      (optsLabel opts)
      ( ( if typ == FieldTypeImage
            then do
              src <-
                maybeToList
                  $ st
                  ^? cloneTraversal optic
                  . #fieldInput
                  . #uniqueValue
              if null src
                then mempty
                else
                  [ input_
                      $ catMaybes
                        [ Just $ type_ "file",
                          Just $ accept_ "image/*",
                          Just $ onInput onInputAction,
                          fmap required_
                            $ st
                            ^? cloneTraversal optic
                            . #fieldRequired
                        ],
                    img_
                      ( loading_ "lazy"
                          : src_ src
                          : optsExtraAttributes opts
                      ),
                    br_ mempty
                  ]
            else
              singleton
                . input_
                $ ( catMaybes
                      [ Just . type_ $ htmlFieldType typ,
                        fmap required_
                          $ st
                          ^? cloneTraversal optic
                          . #fieldRequired,
                        fmap
                          (textProp "defaultValue")
                          ( st
                              ^? cloneTraversal
                                optic
                              . #fieldInput
                              . #uniqueValue
                          ),
                        Just
                          $ onInput onInputAction,
                        Just
                          . disabled_
                          $ opts
                          ^. #optsDisabled,
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
                        Just
                          . onKeyDown
                          $ action
                          . optsOnKeyDownAction opts uid,
                        Just
                          $ onBlur onBlurAction
                      ]
                  )
                <> ( opts ^. #optsExtraAttributes
                   )
        )
          --
          -- TODO : with new semantic layout separate leading/trailing
          -- widgets do not make a lot of sense, should be a single option
          -- which is just a list widgets.
          --
          <> catMaybes
            [ fmap
                (fieldIcon full opts)
                ( opts
                    ^? #optsLeadingWidget
                    . _Just
                    . cloneTraversal widgetOptic
                ),
              fmap
                (fieldIcon full opts)
                ( opts
                    ^? #optsTrailingWidget
                    . _Just
                    . cloneTraversal widgetOptic
                )
            ]
      )
  where
    st = argsModel args
    optic = argsOptic args
    action = argsAction args
    placeholder = optsPlaceholder opts
    widgetOptic =
      if null . fromMaybe mempty $ getInput st
        then #optsWidgetPairEmpty
        else #optsWidgetPairNonEmpty
    typ =
      fromMaybe FieldTypeText
        $ st
        ^? cloneTraversal optic
        . #fieldType
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
    fieldIconSimple opts Icon.IconCopy mempty
      . action
      $ case st ^? cloneTraversal optic . #fieldInput . #uniqueValue of
        Nothing -> PureUpdate id
        Just txt -> Jsm.shareText txt
  ClearWidget ->
    fieldIconSimple opts Icon.IconClose mempty
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
    fieldIconSimple opts Icon.IconPaste mempty
      $ insertAction full Jsm.selectClipboard
  ScanQrWidget ->
    fieldIconSimple opts Icon.IconQrCode mempty
      $ insertAction full Jsm.selectBarcode
  ShowOrHideWidget ->
    case st ^? cloneTraversal optic . #fieldType of
      Just FieldTypePassword ->
        fieldIconSimple opts Icon.IconHidden mempty
          . action
          . PureUpdate
          $ cloneTraversal optic
          . #fieldType
          .~ FieldTypeText
      _ ->
        fieldIconSimple opts Icon.IconVisible mempty
          . action
          . PureUpdate
          $ cloneTraversal optic
          . #fieldType
          .~ FieldTypePassword
  UpWidget opt idx attrs ->
    fieldIconSimple opts Icon.IconUp attrs
      . action
      $ Jsm.moveUp opt idx
  DownWidget opt idx attrs ->
    fieldIconSimple opts Icon.IconDown attrs
      . action
      $ Jsm.moveDown opt idx
  DeleteWidget opt idx attrs ->
    fieldIconSimple opts Icon.IconDelete attrs
      . action
      $ Jsm.removeAt opt idx
  ModalWidget (ModalItemWidget opt idx _ _ ooc) ->
    fieldIconSimple opts Icon.IconSettings mempty
      . action
      . PureUpdate
      $ cloneTraversal opt
      . ix idx
      . cloneTraversal ooc
      .~ Opened
  ModalWidget (ModalFieldWidget opt idx access _) ->
    fieldIconSimple opts Icon.IconSettings mempty
      . action
      . PureUpdate
      $ cloneTraversal opt
      . ix idx
      . cloneTraversal access
      . #fieldModalState
      .~ Opened
  ModalWidget (ModalMiniWidget opt) ->
    fieldIconSimple opts Icon.IconSettings mempty
      . action
      . PureUpdate
      $ cloneTraversal opt
      . #fieldModalState
      .~ Opened
  ActionWidget icon attrs act ->
    fieldIconSimple opts icon attrs act
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
  Opts model action ->
  Icon.Icon ->
  [Attribute action] ->
  action ->
  View action
fieldIconSimple opts i attrs action =
  button_
    ( [onClick action]
        <> attrs
    )
    [ optsIcon opts i
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
          textField
            Args
              { argsModel = args ^. #argsModel,
                argsOptic = cloneTraversal opt . ix idx . cloneTraversal lbl,
                argsAction = args ^. #argsAction,
                argsEmitter = args ^. #argsEmitter
              }
            ( defOpts
                & #optsPlaceholder
                .~ "Label"
            )
            <> [ button_
                  [ onClick
                      . action
                      . Jsm.addFieldPair
                      $ cloneTraversal opt
                      . ix idx
                      . cloneTraversal fps
                  ]
                  [text "Add note"],
                 button_
                  [onClick . action $ Jsm.moveDown opt idx]
                  [text "Down"],
                 button_
                  [onClick . action $ Jsm.moveUp opt idx]
                  [text "Up"],
                 button_
                  [onClick . action $ Jsm.duplicateAt opt idx]
                  [text "Clone"],
                 button_
                  [onClick . action $ Jsm.removeAt opt idx]
                  [text "Delete"]
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
            <> [ -- Switch.switch
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
                 button_
                  [onClick . action $ Jsm.moveDown opt idx]
                  [text "Down"],
                 button_
                  [onClick . action $ Jsm.moveUp opt idx]
                  [text "Up"],
                 button_
                  [onClick . action $ Jsm.duplicateAt opt idx]
                  [text "Clone"],
                 button_
                  [onClick . action $ Jsm.removeAt opt idx]
                  [text "Delete"]
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
          [ selectTypeWidget args opt
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
  Opts model action ->
  Args model action t f ->
  [View action]
fieldViewer opts args =
  case typ of
    FieldTypeNumber -> genericFieldViewer opts args text
    FieldTypePercent -> genericFieldViewer opts args $ text . (<> "%")
    FieldTypeText -> genericFieldViewer opts args text
    FieldTypeTitle -> header val
    FieldTypeImage -> genericFieldViewer opts args text
    FieldTypeHtml ->
      genericFieldViewer
        opts
        ( args
            & cloneTraversal optic
            . #fieldOpts
            . #fieldOptsTruncateLimit
            .~ Nothing
        )
        rawHtml
    FieldTypePassword ->
      genericFieldViewer opts args
        $ const "*****"
    FieldTypeQrCode ->
      genericFieldViewer
        opts
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
  Opts model action ->
  Args model action t f ->
  (Unicode -> View action) ->
  [View action]
genericFieldViewer opts0 args widget =
  if input == mempty
    then mempty
    else
      ( case stateQr of
          Opened -> Qr.qr input
          Closed -> mempty
      )
        <> ( optsLeftRightViewer
              opts0
              ( if typ == FieldTypeImage
                  then
                    if null input
                      then mempty
                      else
                        [ img_ [loading_ "lazy", src_ input]
                        ]
                  else
                    [ widget
                        $ truncateFieldViewer
                          allowTrunc
                          stateTrunc
                          (opts ^. #fieldOptsTruncateLimit)
                          input
                    ]
              )
              ( if null extraWidgets
                  then mempty
                  else extraWidgets
              )
           )
  where
    st = argsModel args
    optic = argsOptic args
    action = argsAction args
    typ =
      fromMaybe FieldTypeText
        $ st
        ^? cloneTraversal (argsOptic args)
        . #fieldType
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
                  Closed -> Icon.IconExpand
                  Opened -> Icon.IconCollapse
            pure
              . fieldViewerIcon opts0 icon
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
                      Closed -> Icon.IconQrCode
                      Opened -> Icon.IconFile
                pure
                  . fieldViewerIcon opts0 icon
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
                [ fieldViewerIcon opts0 Icon.IconCopy
                    . action
                    $ Jsm.shareText input
                ]
           )

fieldViewerIcon :: Opts model action -> Icon.Icon -> action -> View action
fieldViewerIcon opts icon action =
  button_
    [onClick action]
    [optsIcon opts icon]

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
  action . EffectUpdate . selector $ \case
    Nothing -> Jsm.popupText @Unicode "Failure!"
    Just inp -> do
      let updateInput =
            cloneTraversal optic
              . #fieldInput
              . #uniqueValue
              .~ inp
      case updateInput prev ^? cloneTraversal optic >>= parser of
        Nothing -> Jsm.popupText @Unicode "Failure!"
        Just out ->
          emitter
            . PureAndEffectUpdate
              ( updateInput
                  . (cloneTraversal optic . #fieldOutput .~ out)
              )
            $ Jsm.popupText @Unicode "Success!"
  where
    prev = args ^. #argsModel
    optic = args ^. #argsOptic
    action = args ^. #argsAction
    emitter = args ^. #argsEmitter
