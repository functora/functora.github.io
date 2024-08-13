module Functora.Miso.Widgets.Field
  ( Args (..),
    Full (..),
    Opts (..),
    defOpts,
    OptsWidget (..),
    ModalWidget' (..),
    field,
    ratioField,
    textField,
    dynamicField,
    passwordField,
    constTextField,
    dynamicFieldViewer,
  )
where

import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Widgets.Qr as Qr
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography

data Args model action item = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Field item Unique),
    argsAction :: JSM (model -> model) -> action
  }
  deriving stock (Generic)

data Full model action item = Full
  { fullArgs :: Args model action item,
    fullParser :: Field item Unique -> Maybe item,
    fullViewer :: item -> MisoString
  }
  deriving stock (Generic)

data Opts model action = Opts
  { optsDisabled :: Bool,
    optsFullWidth :: Bool,
    optsPlaceholder :: MisoString,
    optsExtraOnInput :: model -> model,
    optsLeadingWidget :: Maybe (OptsWidget model action),
    optsTrailingWidget :: Maybe (OptsWidget model action),
    optsOnKeyDownAction :: Uid -> KeyCode -> JSM (model -> model),
    optsExtraAttributes :: [Attribute action],
    optsFilledOrOutlined :: FilledOrOutlined
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsDisabled = False,
      optsFullWidth = True,
      optsPlaceholder = mempty,
      optsExtraOnInput = id,
      optsLeadingWidget = Just CopyWidget,
      optsTrailingWidget = Just ClearWidget,
      optsOnKeyDownAction = Jsm.enterOrEscapeBlur,
      optsExtraAttributes = mempty,
      optsFilledOrOutlined = Filled
    }

data OptsWidget model action
  = CopyWidget
  | ClearWidget
  | ShowOrHideWidget
  | ModalWidget (ModalWidget' model)
  | ActionWidget MisoString [Attribute action] action
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
    ATraversal' a (Field MisoString Unique) ->
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
  Full model action item ->
  Opts model action ->
  View action
field Full {fullArgs = args, fullParser = parser, fullViewer = viewer} opts =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      LayoutGrid.alignMiddle,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    $ ( do
          x0 <-
            catMaybes
              [ opts ^. #optsLeadingWidget,
                opts ^. #optsTrailingWidget
              ]
          x1 <-
            case x0 of
              ModalWidget w -> pure w
              _ -> mempty
          fieldModal args x1
      )
    <> [ case opts ^. #optsFilledOrOutlined of
          Filled -> TextField.filled
          Outlined -> TextField.outlined
          $ TextField.config
          & TextField.setType
            ( fmap htmlFieldType (st ^? cloneTraversal optic . #fieldType)
            )
          & TextField.setOnInput onInputAction
          & TextField.setDisabled (opts ^. #optsDisabled)
          & TextField.setLabel
            ( Just $ opts ^. #optsPlaceholder
            )
          & TextField.setLeadingIcon
            ( fmap
                (fieldIcon Leading args opts)
                (opts ^. #optsLeadingWidget)
            )
          & TextField.setTrailingIcon
            ( fmap
                (fieldIcon Trailing args opts)
                (opts ^. #optsTrailingWidget)
            )
          & TextField.setAttributes
            ( [ id_ $ htmlUid @MisoString uid,
                onKeyDown $ action . optsOnKeyDownAction opts uid,
                onBlur onBlurAction
              ]
                <> ( if opts ^. #optsFullWidth
                      then [class_ "fill"]
                      else mempty
                   )
                <> ( opts ^. #optsExtraAttributes
                   )
            )
       ]
  where
    st = argsModel args
    optic = argsOptic args
    action = argsAction args
    uid =
      fromMaybe nilUid
        $ st
        ^? cloneTraversal optic
        . #fieldInput
        . #uniqueUid
    getInput st' =
      st' ^? cloneTraversal optic . #fieldInput . #uniqueValue
    getOutput st' = do
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
      action . pure $ \prev ->
        prev
          & cloneTraversal optic
          . #fieldInput
          . #uniqueValue
          %~ maybe id (const . viewer) (getInputReplacement prev)
          & cloneTraversal optic
          . #fieldOutput
          %~ maybe id (const . id) (getOutput prev)
    onInputAction txt =
      action . pure $ \prev ->
        let next =
              prev
                & cloneTraversal optic
                . #fieldInput
                . #uniqueValue
                .~ txt
                & (opts ^. #optsExtraOnInput)
         in next
              & cloneTraversal optic
              . #fieldOutput
              %~ maybe id (const . id) (getOutput next)

ratioField ::
  Args model action Rational ->
  Opts model action ->
  View action
ratioField args =
  field
    Full
      { fullArgs = args,
        fullParser = parseRatio . (^. #fieldInput . #uniqueValue),
        fullViewer = inspectRatioDef
      }

textField ::
  Args model action MisoString ->
  Opts model action ->
  View action
textField args =
  field
    Full
      { fullArgs = args,
        fullParser = Just . (^. #fieldInput . #uniqueValue),
        fullViewer = id
      }

dynamicField ::
  Args model action DynamicField ->
  Opts model action ->
  View action
dynamicField args =
  field
    Full
      { fullArgs = args,
        fullParser = parseDynamicField,
        fullViewer = inspectDynamicField
      }

passwordField ::
  Args model action MisoString ->
  Opts model action ->
  View action
passwordField args opts =
  textField
    args
    ( opts
        & #optsPlaceholder
        .~ ("Password" :: MisoString)
        & #optsLeadingWidget
        .~ Just ShowOrHideWidget
    )

fieldIcon ::
  LeadingOrTrailing ->
  Args model action item ->
  Opts model action ->
  OptsWidget model action ->
  TextField.Icon action
fieldIcon lot args opts = \case
  CopyWidget ->
    fieldIconSimple lot "content_copy" mempty
      . action
      $ case st ^? cloneTraversal optic . #fieldInput . #uniqueValue of
        Nothing -> pure id
        Just txt -> Jsm.shareText txt
  ClearWidget ->
    fieldIconSimple lot "close" mempty . action $ do
      focus
        . toMisoString
        $ htmlUid @MisoString uid
      void
        . JS.eval @MisoString
        $ "var el = document.getElementById('"
        <> htmlUid uid
        <> "'); if (el) el.value = '';"
      pure $ \prev ->
        prev
          & cloneTraversal optic
          . #fieldInput
          . #uniqueValue
          .~ mempty
          & extraOnInput
  ShowOrHideWidget ->
    case st ^? cloneTraversal optic . #fieldType of
      Just FieldTypePassword ->
        fieldIconSimple lot "visibility_off" mempty
          . action
          $ pure (& cloneTraversal optic . #fieldType .~ FieldTypeText)
      _ ->
        fieldIconSimple lot "visibility" mempty
          . action
          $ pure (& cloneTraversal optic . #fieldType .~ FieldTypePassword)
  UpWidget opt idx attrs ->
    fieldIconSimple lot "keyboard_double_arrow_up" attrs
      . action
      $ Jsm.moveUp opt idx
  DownWidget opt idx attrs ->
    fieldIconSimple lot "keyboard_double_arrow_down" attrs
      . action
      $ Jsm.moveDown opt idx
  DeleteWidget opt idx attrs ->
    fieldIconSimple lot "delete_forever" attrs
      . action
      $ Jsm.removeAt opt idx
  ModalWidget (ModalItemWidget opt idx _ _ ooc) ->
    fieldIconSimple lot "settings" [Theme.primary]
      . action
      $ pure
        ( &
            cloneTraversal opt
              . ix idx
              . cloneTraversal ooc
              .~ Opened
        )
  ModalWidget (ModalFieldWidget opt idx access _) ->
    fieldIconSimple lot "settings" mempty
      . action
      $ pure
        ( &
            cloneTraversal opt
              . ix idx
              . cloneTraversal access
              . #fieldModalState
              .~ Opened
        )
  ModalWidget (ModalMiniWidget opt) ->
    fieldIconSimple lot "settings" mempty
      . action
      $ pure
        ( &
            cloneTraversal opt
              . #fieldModalState
              .~ Opened
        )
  ActionWidget icon attrs act ->
    fieldIconSimple lot icon attrs act
  where
    st = args ^. #argsModel
    optic = args ^. #argsOptic
    action = args ^. #argsAction
    extraOnInput = opts ^. #optsExtraOnInput
    uid =
      fromMaybe nilUid
        $ st
        ^? cloneTraversal optic
        . #fieldInput
        . #uniqueUid

fieldIconSimple ::
  LeadingOrTrailing ->
  MisoString ->
  [Attribute action] ->
  action ->
  TextField.Icon action
fieldIconSimple lot txt attrs action =
  TextField.icon
    ( [ class_ $ case lot of
          Leading -> "mdc-text-field__icon--leading"
          Trailing -> "mdc-text-field__icon--trailing",
        style_ [("pointer-events", "auto")],
        textProp "role" "button",
        intProp "tabindex" 0,
        onClick action
      ]
        <> attrs
    )
    txt

fieldModal :: Args model action item -> ModalWidget' model -> [View action]
fieldModal args@Args {argsAction = action} (ModalItemWidget opt idx fps lbl ooc) =
  Dialog.dialog
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
                    argsAction = args ^. #argsAction
                  }
                ( defOpts
                    & #optsPlaceholder
                    .~ "Label"
                ),
            Grid.mediumCell
              $ Button.raised
                ( Button.config
                    & Button.setOnClick
                      ( action
                          . Jsm.addFieldPair
                          $ cloneTraversal opt
                          . ix idx
                          . cloneTraversal fps
                      )
                    & Button.setIcon
                      ( Just "add"
                      )
                    & Button.setAttributes
                      [ class_ "fill",
                        Theme.secondaryBg
                      ]
                )
                "Add note",
            Grid.smallCell
              $ Button.raised
                ( Button.config
                    & Button.setOnClick
                      ( action $ Jsm.moveDown opt idx
                      )
                    & Button.setIcon
                      ( Just "keyboard_double_arrow_down"
                      )
                    & Button.setAttributes
                      [ class_ "fill",
                        Theme.secondaryBg
                      ]
                )
                "Down",
            Grid.smallCell
              $ Button.raised
                ( Button.config
                    & Button.setOnClick
                      ( action $ Jsm.moveUp opt idx
                      )
                    & Button.setIcon
                      ( Just "keyboard_double_arrow_up"
                      )
                    & Button.setAttributes
                      [ class_ "fill",
                        Theme.secondaryBg
                      ]
                )
                "Up",
            Grid.smallCell
              $ Button.raised
                ( Button.config
                    & Button.setOnClick
                      ( action $ Jsm.duplicateAt opt idx
                      )
                    & Button.setIcon
                      ( Just "library_add"
                      )
                    & Button.setAttributes
                      [ class_ "fill",
                        Theme.secondaryBg
                      ]
                )
                "Clone",
            Grid.smallCell
              $ Button.raised
                ( Button.config
                    & Button.setOnClick
                      ( action $ Jsm.removeAt opt idx
                      )
                    & Button.setIcon
                      ( Just "delete_forever"
                      )
                    & Button.setAttributes
                      [ class_ "fill",
                        Theme.secondaryBg
                      ]
                )
                "Delete"
          ]
      }
fieldModal args (ModalFieldWidget opt idx access sod) = do
  let optic =
        cloneTraversal opt
          . ix idx
          . cloneTraversal access
  let action =
        args ^. #argsAction
  Dialog.dialog
    Dialog.Args
      { Dialog.argsModel = args ^. #argsModel,
        Dialog.argsOptic = cloneTraversal optic . #fieldModalState,
        Dialog.argsAction = args ^. #argsAction,
        Dialog.argsContent =
          ( case sod of
              Static -> mempty
              Dynamic ->
                [ let typ :| typs = enumerateNE @FieldType
                   in Grid.bigCell
                        $ Select.outlined
                          ( Select.config
                              & Select.setLabel
                                ( Just "Type"
                                )
                              & Select.setAttributes
                                [ class_ "fill-inner"
                                ]
                              & Select.setSelected
                                ( args
                                    ^? #argsModel
                                    . cloneTraversal optic
                                    . #fieldType
                                )
                              & Select.setOnChange
                                ( \x ->
                                    args
                                      ^. #argsAction
                                      $ pure
                                        ( &
                                            cloneTraversal optic
                                              . #fieldType
                                              .~ x
                                        )
                                )
                          )
                          ( SelectItem.selectItem
                              (SelectItem.config typ)
                              [text $ userFieldType typ]
                          )
                        $ fmap
                          ( \t ->
                              SelectItem.selectItem
                                (SelectItem.config t)
                                [text $ userFieldType t]
                          )
                          typs
                ]
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
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( action $ Jsm.moveDown opt idx
                          )
                        & Button.setIcon
                          ( Just "keyboard_double_arrow_down"
                          )
                        & Button.setAttributes
                          [ class_ "fill",
                            Theme.secondaryBg
                          ]
                    )
                    "Down",
                 Grid.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( action $ Jsm.moveUp opt idx
                          )
                        & Button.setIcon
                          ( Just "keyboard_double_arrow_up"
                          )
                        & Button.setAttributes
                          [ class_ "fill",
                            Theme.secondaryBg
                          ]
                    )
                    "Up",
                 Grid.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( action $ Jsm.duplicateAt opt idx
                          )
                        & Button.setIcon
                          ( Just "library_add"
                          )
                        & Button.setAttributes
                          [ class_ "fill",
                            Theme.secondaryBg
                          ]
                    )
                    "Clone",
                 Grid.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( action $ Jsm.removeAt opt idx
                          )
                        & Button.setIcon
                          ( Just "delete_forever"
                          )
                        & Button.setAttributes
                          [ class_ "fill",
                            Theme.secondaryBg
                          ]
                    )
                    "Delete"
               ]
      }
fieldModal args (ModalMiniWidget opt) =
  Dialog.dialog
    Dialog.Args
      { Dialog.argsModel = args ^. #argsModel,
        Dialog.argsOptic = cloneTraversal opt . #fieldModalState,
        Dialog.argsAction = args ^. #argsAction,
        Dialog.argsContent =
          [ Grid.bigCell
              $ selectTypeWidget args opt
          ]
      }

selectTypeWidget ::
  Args model action item ->
  ATraversal' model (Field a Unique) ->
  View action
selectTypeWidget args optic =
  let typ :| typs = enumerateNE @FieldType
   in Select.outlined
        ( Select.config
            & Select.setLabel
              ( Just "Type"
              )
            & Select.setAttributes
              [ class_ "fill-inner"
              ]
            & Select.setSelected
              ( args
                  ^? #argsModel
                  . cloneTraversal optic
                  . #fieldType
              )
            & Select.setOnChange
              ( \x ->
                  args
                    ^. #argsAction
                    $ pure
                      ( &
                          cloneTraversal optic
                            . #fieldType
                            .~ x
                      )
              )
        )
        ( SelectItem.selectItem
            (SelectItem.config typ)
            [text $ userFieldType typ]
        )
        $ fmap
          ( \t ->
              SelectItem.selectItem
                (SelectItem.config t)
                [text $ userFieldType t]
          )
          typs

constTextField ::
  MisoString ->
  Opts model action ->
  (JSM (model -> model) -> action) ->
  View action
constTextField txt opts action =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      LayoutGrid.alignMiddle,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ case opts ^. #optsFilledOrOutlined of
        Filled -> TextField.filled
        Outlined -> TextField.outlined
        $ TextField.config
        & TextField.setDisabled True
        & TextField.setValue (Just txt)
        & TextField.setType
          ( Just $ htmlFieldType FieldTypeText
          )
        & TextField.setLabel
          ( Just $ opts ^. #optsPlaceholder
          )
        & TextField.setLeadingIcon
          ( fmap
              ( fieldIconSimple Leading "content_copy" mempty . \case
                  CopyWidget -> action $ Jsm.shareText txt
                  _ -> error "constTextField unsupported widget"
              )
              (opts ^. #optsLeadingWidget)
          )
        & TextField.setTrailingIcon
          ( fmap
              ( \case
                  ActionWidget icon attrs act ->
                    fieldIconSimple Trailing icon attrs act
                  _ ->
                    error "constTextField unsupported widget"
              )
              (opts ^. #optsTrailingWidget)
          )
        & TextField.setAttributes
          ( ( if opts ^. #optsFullWidth
                then [class_ "fill"]
                else mempty
            )
              <> ( opts ^. #optsExtraAttributes
                 )
          )
    ]

--
-- TODO : support optional copying widgets
--
dynamicFieldViewer ::
  forall model action.
  (JSM (model -> model) -> action) ->
  Field DynamicField Unique ->
  [View action]
dynamicFieldViewer action value =
  case value ^. #fieldType of
    FieldTypeNumber -> plain out text
    FieldTypePercent -> plain out $ text . (<> "%")
    FieldTypeText -> plain out text
    FieldTypeTitle -> header out
    FieldTypeHtml -> plain out rawHtml
    FieldTypePassword -> plain out $ const "*****"
    FieldTypeQrCode ->
      Qr.qr
        Qr.Args
          { Qr.argsValue = out,
            Qr.argsAction = action
          }
        $ Qr.defOpts @action
        & #optsAllowCopy
        .~ allowCopy
  where
    out = inspectDynamicField $ value ^. #fieldOutput
    allowCopy = value ^. #fieldAllowCopy

plain ::
  ( Eq a,
    Monoid a,
    ToMisoString a
  ) =>
  a ->
  (MisoString -> View action) ->
  [View action]
plain out widget =
  if out == mempty
    then mempty
    else
      [ span_
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
          [widget $ toMisoString out]
      ]

header :: MisoString -> [View action]
header txt =
  if txt == mempty
    then mempty
    else
      [ div_
          [ Typography.headline5,
            class_ "fill",
            style_ [("text-align", "center")]
          ]
          [ text txt
          ]
      ]
