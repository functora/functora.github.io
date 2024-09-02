module Functora.Miso.Widgets.Field
  ( Args (..),
    Full (..),
    Opts (..),
    defOpts,
    OptsWidget (..),
    ModalWidget' (..),
    ViewerArgs (..),
    ViewerOpts (..),
    defViewerOpts,
    field,
    ratioField,
    textField,
    dynamicField,
    passwordField,
    constTextField,
    fieldViewer,
  )
where

import qualified Data.Text as T
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Widgets.Qr as Qr
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.IconButton as IconButton
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography

data Args model action item = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Field item Unique),
    argsAction :: (model -> JSM model) -> action
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
    optsOnInputAction :: Maybe ((model -> JSM model) -> action),
    optsLeadingWidget :: Maybe (OptsWidget model action),
    optsTrailingWidget :: Maybe (OptsWidget model action),
    optsOnKeyDownAction :: Uid -> KeyCode -> model -> JSM model,
    optsExtraAttributes :: [Attribute action],
    optsFilledOrOutlined :: FilledOrOutlined
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsDisabled = False,
      optsFullWidth = False,
      optsPlaceholder = mempty,
      optsOnInputAction = Nothing,
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

data ViewerArgs model action t f = ViewerArgs
  { viewerArgsModel :: model,
    viewerArgsOptic :: Getter' model (Field t f),
    viewerArgsAction :: (model -> JSM model) -> action
  }
  deriving stock (Generic)

data ViewerOpts model = ViewerOpts
  { viewerOptsQrOptic :: Maybe (ATraversal' model OpenedOrClosed),
    viewerOptsTruncateOptic :: Maybe (ATraversal' model OpenedOrClosed),
    viewerOptsTruncateLimit :: Maybe Int
  }
  deriving stock (Generic)

defViewerOpts :: ViewerOpts model
defViewerOpts =
  ViewerOpts
    { viewerOptsQrOptic = Nothing,
      viewerOptsTruncateOptic = Nothing,
      viewerOptsTruncateLimit = Just defTruncateLimit
    }

defTruncateLimit :: Int
defTruncateLimit = 67

field ::
  Full model action item ->
  Opts model action ->
  View action
field Full {fullArgs = args, fullParser = parser, fullViewer = viewer} opts =
  cell opts
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
                onBlur onBlurAction,
                Css.fullWidth
              ]
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
      action $ \prev ->
        pure
          $ prev
          & cloneTraversal optic
          . #fieldInput
          . #uniqueValue
          %~ maybe id (const . viewer) (getInputReplacement prev)
          & cloneTraversal optic
          . #fieldOutput
          %~ maybe id (const . id) (getOutput prev)
    onInputAction txt =
      fromMaybe action (optsOnInputAction opts) $ \prev ->
        let next =
              prev
                & cloneTraversal optic
                . #fieldInput
                . #uniqueValue
                .~ txt
         in pure
              $ next
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
        Nothing -> pure . id
        Just txt -> Jsm.shareText txt
  ClearWidget ->
    fieldIconSimple lot "close" mempty
      . ( fromMaybe action
            $ optsOnInputAction opts
        )
      $ \prev -> do
        focus
          . toMisoString
          $ htmlUid @MisoString uid
        void
          . JS.eval @MisoString
          $ "var el = document.getElementById('"
          <> htmlUid uid
          <> "'); if (el) el.value = '';"
        pure
          $ prev
          & cloneTraversal optic
          . #fieldInput
          . #uniqueValue
          .~ mempty
  ShowOrHideWidget ->
    case st ^? cloneTraversal optic . #fieldType of
      Just FieldTypePassword ->
        fieldIconSimple lot "visibility_off" mempty
          . action
          $ pure
          . (& cloneTraversal optic . #fieldType .~ FieldTypeText)
      _ ->
        fieldIconSimple lot "visibility" mempty
          . action
          $ pure
          . (& cloneTraversal optic . #fieldType .~ FieldTypePassword)
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
      . ( &
            cloneTraversal opt
              . ix idx
              . cloneTraversal ooc
              .~ Opened
        )
  ModalWidget (ModalFieldWidget opt idx access _) ->
    fieldIconSimple lot "settings" mempty
      . action
      $ pure
      . ( &
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
      . ( &
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
              [ textField
                  Args
                    { argsModel = args ^. #argsModel,
                      argsOptic = cloneTraversal opt . ix idx . cloneTraversal lbl,
                      argsAction = args ^. #argsAction
                    }
                  ( defOpts
                      & #optsPlaceholder
                      .~ "Label"
                  )
              ],
            Grid.mediumCell
              [ Button.raised
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
                        [ Css.fullWidth,
                          Theme.secondaryBg
                        ]
                  )
                  "Add note"
              ],
            Grid.smallCell
              [ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( action $ Jsm.moveDown opt idx
                        )
                      & Button.setIcon
                        ( Just "keyboard_double_arrow_down"
                        )
                      & Button.setAttributes
                        [ Css.fullWidth,
                          Theme.secondaryBg
                        ]
                  )
                  "Down"
              ],
            Grid.smallCell
              [ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( action $ Jsm.moveUp opt idx
                        )
                      & Button.setIcon
                        ( Just "keyboard_double_arrow_up"
                        )
                      & Button.setAttributes
                        [ Css.fullWidth,
                          Theme.secondaryBg
                        ]
                  )
                  "Up"
              ],
            Grid.smallCell
              [ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( action $ Jsm.duplicateAt opt idx
                        )
                      & Button.setIcon
                        ( Just "library_add"
                        )
                      & Button.setAttributes
                        [ Css.fullWidth,
                          Theme.secondaryBg
                        ]
                  )
                  "Clone"
              ],
            Grid.smallCell
              [ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( action $ Jsm.removeAt opt idx
                        )
                      & Button.setIcon
                        ( Just "delete_forever"
                        )
                      & Button.setAttributes
                        [ Css.fullWidth,
                          Theme.secondaryBg
                        ]
                  )
                  "Delete"
              ]
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
                        [ Select.outlined
                            ( Select.config
                                & Select.setLabel
                                  ( Just "Type"
                                  )
                                & Select.setSelected
                                  ( args
                                      ^? #argsModel
                                      . cloneTraversal optic
                                      . #fieldType
                                  )
                                & Select.setOnChange
                                  ( \x ->
                                      action
                                        $ pure
                                        . ( &
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
                  [ Button.raised
                      ( Button.config
                          & Button.setOnClick
                            ( action $ Jsm.moveDown opt idx
                            )
                          & Button.setIcon
                            ( Just "keyboard_double_arrow_down"
                            )
                          & Button.setAttributes
                            [ Css.fullWidth,
                              Theme.secondaryBg
                            ]
                      )
                      "Down"
                  ],
                 Grid.smallCell
                  [ Button.raised
                      ( Button.config
                          & Button.setOnClick
                            ( action $ Jsm.moveUp opt idx
                            )
                          & Button.setIcon
                            ( Just "keyboard_double_arrow_up"
                            )
                          & Button.setAttributes
                            [ Css.fullWidth,
                              Theme.secondaryBg
                            ]
                      )
                      "Up"
                  ],
                 Grid.smallCell
                  [ Button.raised
                      ( Button.config
                          & Button.setOnClick
                            ( action $ Jsm.duplicateAt opt idx
                            )
                          & Button.setIcon
                            ( Just "library_add"
                            )
                          & Button.setAttributes
                            [ Css.fullWidth,
                              Theme.secondaryBg
                            ]
                      )
                      "Clone"
                  ],
                 Grid.smallCell
                  [ Button.raised
                      ( Button.config
                          & Button.setOnClick
                            ( action $ Jsm.removeAt opt idx
                            )
                          & Button.setIcon
                            ( Just "delete_forever"
                            )
                          & Button.setAttributes
                            [ Css.fullWidth,
                              Theme.secondaryBg
                            ]
                      )
                      "Delete"
                  ]
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
              [ selectTypeWidget args opt
              ]
          ]
      }

selectTypeWidget ::
  Args model action item ->
  ATraversal' model (Field a Unique) ->
  View action
selectTypeWidget args@Args {argsAction = action} optic =
  let typ :| typs = enumerateNE @FieldType
   in Select.outlined
        ( Select.config
            & Select.setLabel
              ( Just "Type"
              )
            & Select.setSelected
              ( args
                  ^? #argsModel
                  . cloneTraversal optic
                  . #fieldType
              )
            & Select.setOnChange
              ( \x ->
                  action
                    $ pure
                    . ( &
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
  ((model -> JSM model) -> action) ->
  View action
constTextField txt opts action =
  cell
    opts
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
          ( [Css.fullWidth] <> (opts ^. #optsExtraAttributes)
          )
    ]

cell :: Opts model action -> [View action] -> View action
cell Opts {optsFullWidth = full} =
  LayoutGrid.cell
    $ if full
      then
        [ LayoutGrid.span12Desktop,
          LayoutGrid.span8Tablet,
          LayoutGrid.span4Phone,
          LayoutGrid.alignMiddle,
          style_
            [ ("display", "flex"),
              ("align-items", "center")
            ]
        ]
      else
        [ LayoutGrid.span6Desktop,
          LayoutGrid.span4Tablet,
          LayoutGrid.span4Phone,
          LayoutGrid.alignMiddle,
          style_
            [ ("display", "flex"),
              ("align-items", "center")
            ]
        ]

--
-- TODO : support optional copying widgets
--
fieldViewer ::
  ( Foldable1 f
  ) =>
  ViewerArgs model action t f ->
  ViewerOpts model ->
  [View action]
fieldViewer args opts =
  case value ^. #fieldType of
    FieldTypeNumber -> genericFieldViewer args opts text
    FieldTypePercent -> genericFieldViewer args opts $ text . (<> "%")
    FieldTypeText -> genericFieldViewer args opts text
    FieldTypeTitle -> header input
    FieldTypeHtml ->
      genericFieldViewer args opts {viewerOptsTruncateLimit = Nothing} rawHtml
    FieldTypePassword -> genericFieldViewer args opts $ const "*****"
    FieldTypeQrCode -> Qr.qr input <> genericFieldViewer args opts text
  where
    value = args ^. #viewerArgsModel . viewerArgsOptic args
    input = fold1 $ value ^. #fieldInput

header :: MisoString -> [View action]
header txt =
  if txt == mempty
    then mempty
    else
      [ div_
          [ Typography.headline5,
            Css.fullWidth,
            style_ [("text-align", "center")]
          ]
          [ text txt
          ]
      ]

genericFieldViewer ::
  ( Foldable1 f
  ) =>
  ViewerArgs model action t f ->
  ViewerOpts model ->
  (MisoString -> View action) ->
  [View action]
genericFieldViewer args opts widget =
  if input == mempty
    then mempty
    else
      ( case stateQr of
          Opened -> Qr.qr input
          Closed -> mempty
      )
        <> [ span_
              [ Typography.typography,
                Css.fullWidth,
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
                    ("border-radius", "4px"),
                    ("line-height", "150%")
                  ]
              ]
              [ div_ mempty
                  $ [ widget $ truncateFieldInput allowTrunc stateTrunc opts input
                    ]
                  <> ( if null extraWidgets then mempty else [br_ mempty]
                     )
                  <> extraWidgets
              ]
           ]
  where
    st = args ^. #viewerArgsModel
    value = st ^. viewerArgsOptic args
    input = fold1 $ value ^. #fieldInput
    action = args ^. #viewerArgsAction
    stateQr = fromMaybe Closed $ do
      trav <- opts ^. #viewerOptsQrOptic
      st ^? cloneTraversal trav
    allowCopy =
      value ^. #fieldAllowCopy
    allowTrunc =
      maybe False (length input >)
        $ opts
        ^. #viewerOptsTruncateLimit
    opticTrunc =
      opts ^. #viewerOptsTruncateOptic
    stateTrunc = fromMaybe Closed $ do
      trav <- opticTrunc
      st ^? cloneTraversal trav
    extraWidgets =
      ( if not allowTrunc
          then mempty
          else do
            let icon = case stateTrunc of
                  Closed -> "open_in_full"
                  Opened -> "close_fullscreen"
            trav <- maybeToList opticTrunc
            pure
              . fieldViewerIcon icon
              . action
              $ pure
              . ( &
                    cloneTraversal trav
                      %~ ( \case
                            Closed -> Opened
                            Opened -> Closed
                         )
                )
      )
        <> ( do
              trav <- maybeToList $ opts ^. #viewerOptsQrOptic
              pure
                . fieldViewerIcon "qr_code_2"
                . action
                $ pure
                . ( &
                      cloneTraversal trav
                        %~ ( \case
                              Closed -> Opened
                              Opened -> Closed
                           )
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

fieldViewerIcon :: MisoString -> action -> View action
fieldViewerIcon icon action =
  IconButton.iconButton
    ( IconButton.config
        & IconButton.setOnClick action
        & IconButton.setAttributes [Theme.primary]
    )
    icon

truncateFieldInput ::
  Bool ->
  OpenedOrClosed ->
  ViewerOpts model ->
  MisoString ->
  MisoString
truncateFieldInput True Closed opts raw =
  let full = fromMisoString raw
      half = fromMaybe defTruncateLimit (viewerOptsTruncateLimit opts) `div` 2
   in toMisoString
        $ T.take half full
        <> "..."
        <> T.takeEnd half full
truncateFieldInput _ _ _ raw =
  raw
