module Functora.Miso.Widgets.Field
  ( Args (..),
    Full (..),
    Opts (..),
    defOpts,
    defTrailingWidgets,
    OptsWidget (..),
    ModalWidget' (..),
    truncateUnicode,
    expandDynamicField,
    labeled,
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
    optsFullWidth :: Bool,
    optsPlaceholder :: Unicode,
    optsOnInputAction :: Maybe (Update model -> action),
    optsTrailingWidgets ::
      Unicode ->
      FocusedOrBlurred ->
      EnabledOrDisabled ->
      [OptsWidget model action],
    optsOnKeyDownAction :: Unicode -> KeyCode -> Update model,
    optsExtraAttributes :: [Attribute action],
    optsLeftRightViewer :: [View action] -> [View action] -> [View action],
    optsEnabledOrDisabled :: EnabledOrDisabled,
    optsExtraAttributesImage :: [Attribute action]
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsIcon = Icon.icon @Icon.Fa,
      optsLabel = Nothing,
      optsFullWidth = False,
      optsPlaceholder = mempty,
      optsOnInputAction = Nothing,
      optsTrailingWidgets = defTrailingWidgets,
      optsOnKeyDownAction = Jsm.enterOrEscapeBlur,
      optsExtraAttributes = mempty,
      optsLeftRightViewer = (<>),
      optsEnabledOrDisabled = Enabled,
      optsExtraAttributesImage = mempty
    }

defTrailingWidgets ::
  Unicode ->
  FocusedOrBlurred ->
  EnabledOrDisabled ->
  [OptsWidget model action]
defTrailingWidgets input fob = \case
  Enabled ->
    case fob of
      Focused | null input -> [PasteWidget mempty, ClearWidget hide]
      Focused -> [PasteWidget mempty, ClearWidget mempty]
      Blurred -> [PasteWidget hide, ClearWidget hide]
  Disabled ->
    [PasteWidget hide, ClearWidget hide]
  where
    hide :: [Attribute action]
    hide = [style_ [("display", "none")]]

data OptsWidget model action
  = CopyWidget
  | ClearWidget [Attribute action]
  | PasteWidget [Attribute action]
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

labeled :: Unicode -> [Attribute action] -> [View action] -> View action
labeled label attrs =
  label_
    ( style_
        [ ("display", "flex"),
          ("flex-wrap", "wrap"),
          ("flex-direction", "row"),
          ("align-items", "center")
        ]
        : attrs
    )
    . ( ( span_
            [style_ [("width", "100%")]]
            [text label]
        )
          :
      )

field ::
  Full model action t Unique ->
  Opts model action ->
  [View action]
field full@Full {fullArgs = args, fullParser = parser, fullViewer = viewer} opts =
  ( do
      x0 <- optsTrailingWidgets opts input focused eod
      x1 <- case x0 of
        ModalWidget w -> pure w
        _ -> mempty
      fieldModal args x1
  )
    <> ( case optsLabel opts of
          Nothing -> id
          Just x ->
            singleton
              . keyed ("field-" <> uidTxt)
              . labeled
                x
                [ onBlur onBlurAction,
                  onFocus onFocusAction
                ]
       )
      ( [ input_
            $ ( catMaybes
                  [ Just
                      $ style_
                        [ ("width", "0"),
                          ("flex-grow", "1")
                        ],
                    Just . type_ $ htmlFieldType typ,
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
                      $ optsEnabledOrDisabled opts
                      == Disabled,
                    fmap placeholder_
                      $ if null placeholder
                        then optsLabel opts
                        else Just placeholder,
                    Just $ id_ uidTxt,
                    Just . onKeyDown $ action . optsOnKeyDownAction opts uidTxt
                  ]
              )
            <> ( if isJust $ optsLabel opts
                  then mempty
                  else
                    [ onBlur onBlurAction,
                      onFocus onFocusAction
                    ]
               )
            <> ( opts ^. #optsExtraAttributes
               )
        ]
          <> dummyWidgets
          <> trailingWidgets
          <> ( if typ /= FieldTypeImage
                then mempty
                else do
                  src <-
                    maybeToList
                      $ st
                      ^? cloneTraversal optic
                      . #fieldInput
                      . #uniqueValue
                  [ input_
                      $ [ type_ "file",
                          accept_ "image/*",
                          onInput onInputFileAction,
                          style_ [("width", "100%")],
                          id_ $ "file-" <> uidTxt
                        ]
                    ]
                    <> ( if null src
                          then mempty
                          else
                            [ img_
                                $ loading_ "lazy"
                                : src_ src
                                : optsExtraAttributesImage opts
                            ]
                       )
             )
      )
  where
    st = argsModel args
    optic = argsOptic args
    action = argsAction args
    placeholder = optsPlaceholder opts
    eod = optsEnabledOrDisabled opts
    input =
      fromMaybe mempty $ st ^? cloneTraversal optic . #fieldInput . #uniqueValue
    focused =
      fromMaybe Blurred $ st ^? cloneTraversal optic . #fieldFocusState
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
    uidTxt =
      either impureThrow id
        . decodeUtf8Strict
        . unTagged
        $ htmlUid uid
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
    dummyWidgets =
      [ button_
          ( style_
              [ ("display", "none")
              ]
              : noopAll action
          )
          mempty
      ]
    trailingWidgets =
      fmap
        ( fieldIcon full opts
        )
        ( optsTrailingWidgets opts input focused eod
        )
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
          & cloneTraversal optic
          . #fieldFocusState
          .~ Blurred
    onFocusAction =
      action
        . PureUpdate
        $ cloneTraversal optic
        . #fieldFocusState
        .~ Focused
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
    onInputFileAction =
      const . fromMaybe action (optsOnInputAction opts) . EffectUpdate $ do
        el <- getElementById $ "file-" <> uidTxt
        elExist <- ghcjsPure $ JS.isTruthy el
        if not elExist
          then Jsm.popupText @Unicode "File does not exist!"
          else do
            file <- el JS.! ("files" :: Unicode) JS.!! 0
            Jsm.selectFile
              ( fromMaybe defSelectOpts
                  $ st
                  ^? cloneTraversal optic
                  . #fieldSelectOpts
              )
              file
              $ \case
                Nothing -> Jsm.popupText @Unicode "File is not selected!"
                Just url -> argsEmitter args . PureUpdate $ do
                  let next =
                        st
                          & cloneTraversal optic
                          . #fieldInput
                          . #uniqueValue
                          .~ url
                   in ( cloneTraversal optic
                          . #fieldInput
                          . #uniqueValue
                          .~ url
                      )
                        . ( cloneTraversal optic
                              . #fieldOutput
                              %~ maybe id (const . id) (getOutput next)
                          )

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
        & #optsTrailingWidgets
        --
        -- TODO : add other widgets
        --
        .~ (\_ _ _ -> [ShowOrHideWidget])
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
  ClearWidget attrs ->
    fieldIconSimple opts Icon.IconClose attrs
      . ( fromMaybe action $ optsOnInputAction opts
        )
      $ PureAndEffectUpdate
        ( cloneTraversal optic
            . #fieldInput
            . #uniqueValue
            .~ mempty
        )
        ( do
            uidTxt <-
              either throw pure
                . decodeUtf8Strict
                . unTagged
                $ htmlUid uid
            focus uidTxt
            void
              . JS.eval
              $ "var el = document.getElementById('"
              <> uidTxt
              <> "'); if (el) el.value = '';"
        )
  PasteWidget attrs ->
    fieldIconSimple opts Icon.IconPaste attrs
      . insertAction full
      . Jsm.selectClipboard
      $ fromMaybe
        defSelectOpts
        (st ^? cloneTraversal optic . #fieldSelectOpts)
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
    ( [onMouseDown action]
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
                        [ img_
                            ( loading_ "lazy"
                                : src_ input
                                : optsExtraAttributesImage opts0
                            )
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
              %~ nextEnum
      )
        <> ( if isNothing $ opts ^. #fieldOptsQrState
              then mempty
              else
                pure
                  . fieldViewerIcon opts0 Icon.IconQrCode
                  . action
                  . PureUpdate
                  $ cloneTraversal optic
                  . #fieldOpts
                  . #fieldOptsQrState
                  . _Just
                  %~ nextEnum
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
        <> " ... "
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
            $ do
              whenJust
                ( prev
                    ^? cloneTraversal optic
                    . #fieldInput
                    . #uniqueUid
                )
                $ blur
                . either impureThrow id
                . decodeUtf8Strict
                . unTagged
                . htmlUid
              Jsm.popupText @Unicode
                "Success!"
  where
    prev = args ^. #argsModel
    optic = args ^. #argsOptic
    action = args ^. #argsAction
    emitter = args ^. #argsEmitter
