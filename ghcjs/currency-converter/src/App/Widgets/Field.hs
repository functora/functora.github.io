module App.Widgets.Field
  ( Opts (..),
    OptsWidget (..),
    ModalWidget' (..),
    defOpts,
    field,
    ratioField,
    textField,
    dynamicField,
    passwordField,
    constTextField,
    constLinkField,
    dynamicFieldViewer,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Grid as Grid
import qualified App.Widgets.Icon as Icon
import qualified App.Widgets.Modal as Modal
import qualified App.Widgets.Qr as Qr
import qualified App.Widgets.Select as Select
import Data.Maybe (listToMaybe)
import Functora.Prelude hiding (Field (..), field)
import qualified Language.Javascript.JSaddle as JS
import Miso hiding (URI, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Text.URI as URI
import qualified Prelude

data Opts = Opts
  { optsDisabled :: Bool,
    optsFullWidth :: Bool,
    optsPlaceholder :: Text,
    optsExtraOnInput :: Model -> Model,
    optsLeadingWidget :: Maybe OptsWidget,
    optsTrailingWidget :: Maybe OptsWidget,
    optsOnKeyDownAction :: Uid -> KeyCode -> Action,
    optsExtraAttributes :: [Attribute Action]
  }
  deriving stock (Generic)

data OptsWidget
  = CopyWidget
  | ClearWidget
  | ShowOrHideWidget
  | forall a. UpWidget (ATraversal' Model [a]) Int [Attribute Action]
  | forall a. DownWidget (ATraversal' Model [a]) Int [Attribute Action]
  | forall a. DeleteWidget (ATraversal' Model [a]) Int [Attribute Action]
  | ActionWidget FaIcon [Attribute Action] Action
  | ModalWidget ModalWidget'

data ModalWidget' where
  ModalItemWidget ::
    forall a.
    ( Data a
    ) =>
    ATraversal' Model [a] ->
    Int ->
    ATraversal' a [FieldPair DynamicField Unique] ->
    ATraversal' a (Field Text Unique) ->
    ATraversal' a OpenedOrClosed ->
    ModalWidget'
  ModalFieldWidget ::
    forall a b.
    ( Data a
    ) =>
    ATraversal' Model [a] ->
    Int ->
    ATraversal' a (Field b Unique) ->
    StaticOrDynamic ->
    ModalWidget'
  ModalMiniWidget ::
    forall a.
    ( Data a
    ) =>
    ATraversal' Model (Field a Unique) ->
    ModalWidget'

defOpts :: Opts
defOpts =
  Opts
    { optsDisabled = False,
      optsFullWidth = True,
      optsPlaceholder = mempty,
      optsExtraOnInput = id,
      optsLeadingWidget = Just CopyWidget,
      optsTrailingWidget = Just ClearWidget,
      optsOnKeyDownAction = Misc.onKeyDownAction,
      optsExtraAttributes = mempty
    }

field ::
  Model ->
  ATraversal' Model (Field a Unique) ->
  Opts ->
  (Field a Unique -> Maybe a) ->
  (a -> Text) ->
  View Action
field st optic opts parser viewer =
  Grid.mediumCell
    . (: mempty)
    . div_ [class_ "field"]
    $ ( maybeToList
          . fmap (fieldModal st)
          . listToMaybe
          $ catMaybes
            [ opts ^. #optsLeadingWidget,
              opts ^. #optsTrailingWidget
            ]
          >>= ( \case
                  ModalWidget widget -> [widget]
                  _ -> mempty
              )
      )
    <> [ div_
          ( catMaybes
              [ Just $ class_ "control",
                if isJust (opts ^. #optsLeadingWidget)
                  then Just $ class_ "has-icons-left"
                  else Nothing,
                if isJust (opts ^. #optsTrailingWidget)
                  then Just $ class_ "has-icons-right"
                  else Nothing
              ]
              <> ( if opts ^. #optsFullWidth
                    then [class_ "fill"]
                    else mempty
                 )
              <> ( opts ^. #optsExtraAttributes
                 )
          )
          $ catMaybes
            [ Just
                . input_
                $ catMaybes
                  [ Just $ class_ "input",
                    fmap
                      (type_ . ms . htmlFieldType)
                      (st ^? cloneTraversal optic . #fieldType),
                    Just
                      $ onInput onInputAction,
                    Just
                      . disabled_
                      $ optsDisabled opts,
                    Just
                      . placeholder_
                      . ms
                      $ optsPlaceholder opts
                  ]
                <> [ id_ . ms $ htmlUid @Text uid,
                     onKeyDown . optsOnKeyDownAction opts $ uid,
                     onBlur onBlurAction
                   ]
                <> ( if opts ^. #optsFullWidth
                      then [class_ "fill"]
                      else mempty
                   )
                <> ( opts ^. #optsExtraAttributes
                   ),
              fmap
                (fieldIcon st optic Leading $ opts ^. #optsExtraOnInput)
                (opts ^. #optsLeadingWidget),
              fmap
                (fieldIcon st optic Trailing $ opts ^. #optsExtraOnInput)
                (opts ^. #optsTrailingWidget)
            ]
       ]
  where
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
      PushUpdate $ do
        Misc.verifyUid uid
        pure . ChanItem 300 $ \st' ->
          st'
            & cloneTraversal optic
            . #fieldInput
            . #uniqueValue
            %~ maybe id (const . viewer) (getInputReplacement st')
            & cloneTraversal optic
            . #fieldOutput
            %~ maybe id (const . id) (getOutput st')
    onInputAction txt =
      PushUpdate $ do
        Misc.verifyUid uid
        pure . ChanItem 300 $ \prev ->
          let next =
                prev
                  & cloneTraversal optic
                  . #fieldInput
                  . #uniqueValue
                  .~ fromMisoString txt
                  & (opts ^. #optsExtraOnInput)
           in next
                & cloneTraversal optic
                . #fieldOutput
                %~ maybe id (const . id) (getOutput next)

ratioField ::
  Model ->
  ATraversal' Model (Field Rational Unique) ->
  Opts ->
  View Action
ratioField st optic opts =
  field
    st
    optic
    opts
    ( parseRatio . view (#fieldInput . #uniqueValue)
    )
    inspectRatioDef

textField ::
  Model ->
  ATraversal' Model (Field Text Unique) ->
  Opts ->
  View Action
textField st optic opts =
  field
    st
    optic
    opts
    ( Just . view (#fieldInput . #uniqueValue)
    )
    id

dynamicField ::
  Model ->
  ATraversal' Model [FieldPair DynamicField Unique] ->
  Int ->
  Opts ->
  View Action
dynamicField st optic idx opts =
  field
    st
    ( cloneTraversal optic
        . ix idx
        . #fieldPairValue
    )
    opts
    parseDynamicField
    inspectDynamicField

passwordField ::
  Model ->
  ATraversal' Model (Field Text Unique) ->
  Opts ->
  View Action
passwordField st optic opts =
  textField
    st
    optic
    ( opts
        & #optsPlaceholder
        .~ "Password"
        & #optsLeadingWidget
        .~ Just ShowOrHideWidget
    )

fieldIcon ::
  Model ->
  ATraversal' Model (Field a Unique) ->
  LeadingOrTrailing ->
  ( Model -> Model
  ) ->
  OptsWidget ->
  View Action
fieldIcon st optic lot extraOnInput = \case
  CopyWidget ->
    fieldIconSimple lot FaCopy mempty . PushUpdate $ do
      Misc.verifyUid uid
      whenJust (st ^? cloneTraversal optic . #fieldInput . #uniqueValue)
        $ Misc.copyIntoClipboard st
      pure
        $ ChanItem 0 id
  ClearWidget ->
    fieldIconSimple lot FaClose mempty . PushUpdate $ do
      Misc.verifyUid uid
      focus
        . ms
        $ htmlUid @Text uid
      void
        . JS.eval @Text
        $ "var el = document.getElementById('"
        <> htmlUid uid
        <> "'); if (el) el.value = '';"
      pure . ChanItem 300 $ \st' ->
        st'
          & cloneTraversal optic
          . #fieldInput
          . #uniqueValue
          .~ mempty
          & extraOnInput
  ShowOrHideWidget ->
    case st ^? cloneTraversal optic . #fieldType of
      Just FieldTypePassword ->
        fieldIconSimple lot FaEyeSlash mempty
          $ pureUpdate 0 (& cloneTraversal optic . #fieldType .~ FieldTypeText)
      _ ->
        fieldIconSimple lot FaEye mempty
          $ pureUpdate 0 (& cloneTraversal optic . #fieldType .~ FieldTypePassword)
  UpWidget opt idx attrs ->
    fieldIconSimple lot FaArrowUp attrs
      $ Misc.moveUp opt idx
  DownWidget opt idx attrs ->
    fieldIconSimple lot FaArrowDown attrs
      $ Misc.moveDown opt idx
  DeleteWidget opt idx attrs ->
    fieldIconSimple lot FaTrash attrs
      $ Misc.removeAt opt idx
  ModalWidget (ModalItemWidget opt idx _ _ ooc) ->
    fieldIconSimple lot FaGear mempty
      $ pureUpdate
        0
        ( &
            cloneTraversal opt
              . ix idx
              . cloneTraversal ooc
              .~ Opened
        )
  ModalWidget (ModalFieldWidget opt idx access _) ->
    fieldIconSimple lot FaGear mempty
      $ pureUpdate
        0
        ( &
            cloneTraversal opt
              . ix idx
              . cloneTraversal access
              . #fieldModalState
              .~ Opened
        )
  ModalWidget (ModalMiniWidget opt) ->
    fieldIconSimple lot FaGear mempty
      $ pureUpdate
        0
        ( &
            cloneTraversal opt
              . #fieldModalState
              .~ Opened
        )
  ActionWidget icon attrs action ->
    fieldIconSimple lot icon attrs action
  where
    uid =
      fromMaybe nilUid $ st ^? cloneTraversal optic . #fieldInput . #uniqueUid

fieldIconSimple ::
  forall action.
  LeadingOrTrailing ->
  FaIcon ->
  [Attribute action] ->
  action ->
  View action
fieldIconSimple lot fa attrs action =
  Icon.icon
    ( Icon.defOpts
        & (#optsOnClick :: Lens' (Icon.Opts action) (Maybe action))
        .~ Just action
        & #optsExtraAttributes
        .~ ( [ class_ $ case lot of
                Leading -> "is-left"
                Trailing -> "is-right",
               style_ [("pointer-events", "auto")]
             ]
              <> attrs
           )
    )
    fa

fieldModal ::
  Model ->
  ModalWidget' ->
  View Action
fieldModal st (ModalItemWidget opt idx fps lbl ooc) = do
  let closed =
        pureUpdate
          0
          ( &
              cloneTraversal opt
                . ix idx
                . cloneTraversal ooc
                .~ Closed
          )
  Modal.modal
    st
    Modal.defOpts
    ( cloneTraversal opt
        . ix idx
        . cloneTraversal ooc
    )
    [ Grid.grid
        mempty
        [ Grid.mediumCell
            [ textField
                st
                ( cloneTraversal opt
                    . ix idx
                    . cloneTraversal lbl
                )
                ( defOpts
                    & #optsPlaceholder
                    .~ "Label"
                )
            ],
          Grid.mediumCell
            [ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Add details"
                    & #optsLeadingIcon
                    .~ Just FaPlus
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ ( Just
                          . Misc.newFieldPairAction
                          $ cloneTraversal opt
                          . ix idx
                          . cloneTraversal fps
                       )
                )
            ],
          Grid.smallCell
            [ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Down"
                    & #optsLeadingIcon
                    .~ Just FaArrowDown
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (Misc.moveDown opt idx)
                )
            ],
          Grid.smallCell
            [ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Up"
                    & #optsLeadingIcon
                    .~ Just FaArrowUp
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (Misc.moveUp opt idx)
                )
            ],
          Grid.smallCell
            [ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Clone"
                    & #optsLeadingIcon
                    .~ Just FaClone
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (Misc.duplicateAt opt idx)
                )
            ],
          Grid.smallCell
            [ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Delete"
                    & #optsLeadingIcon
                    .~ Just FaTrash
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just (Misc.removeAt opt idx)
                )
            ],
          Grid.bigCell
            [ Button.button
                ( Button.defOpts
                    & #optsLabel
                    .~ Just @Text "Back"
                    & #optsLeadingIcon
                    .~ Just FaArrowLeft
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just closed
                )
            ]
        ]
    ]
fieldModal st (ModalFieldWidget opt idx access sod) = do
  let optic =
        cloneTraversal opt
          . ix idx
          . cloneTraversal access
  let closed =
        pureUpdate
          0
          ( &
              cloneTraversal opt
                . ix idx
                . cloneTraversal access
                . #fieldModalState
                .~ Closed
          )
  Modal.modal
    st
    Modal.defOpts
    ( cloneTraversal optic
        . #fieldModalState
    )
    [ Grid.grid
        mempty
        $ ( case sod of
              Static -> mempty
              Dynamic -> [selectTypeWidget st optic]
          )
        <> [ Grid.smallCell
              [ Button.button
                  ( Button.defOpts @Action
                      & #optsLabel
                      .~ Just @Text "Down"
                      & #optsLeadingIcon
                      .~ Just FaArrowDown
                      & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                      .~ Just (Misc.moveDown opt idx)
                  )
              ],
             Grid.smallCell
              [ Button.button
                  ( Button.defOpts @Action
                      & #optsLabel
                      .~ Just @Text "Up"
                      & #optsLeadingIcon
                      .~ Just FaArrowUp
                      & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                      .~ Just (Misc.moveUp opt idx)
                  )
              ],
             Grid.smallCell
              [ Button.button
                  ( Button.defOpts @Action
                      & #optsLabel
                      .~ Just @Text "Clone"
                      & #optsLeadingIcon
                      .~ Just FaClone
                      & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                      .~ Just (Misc.duplicateAt opt idx)
                  )
              ],
             Grid.smallCell
              [ Button.button
                  ( Button.defOpts @Action
                      & #optsLabel
                      .~ Just @Text "Delete"
                      & #optsLeadingIcon
                      .~ Just FaTrash
                      & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                      .~ Just (Misc.removeAt opt idx)
                  )
              ],
             Grid.bigCell
              [ Button.button
                  ( Button.defOpts
                      & #optsLabel
                      .~ Just @Text "Back"
                      & #optsLeadingIcon
                      .~ Just FaArrowLeft
                      & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                      .~ Just closed
                  )
              ]
           ]
    ]
fieldModal st (ModalMiniWidget opt) = do
  let closed =
        pureUpdate 0 (& cloneTraversal opt . #fieldModalState .~ Closed)
  Modal.modal
    st
    Modal.defOpts
    ( cloneTraversal opt
        . #fieldModalState
    )
    [ Grid.grid
        mempty
        [ Grid.bigCell [selectTypeWidget st opt],
          Grid.bigCell
            [ Button.button
                ( Button.defOpts @Action
                    & #optsLabel
                    .~ Just @Text "Back"
                    & #optsLeadingIcon
                    .~ Just FaArrowLeft
                    & (#optsOnClick :: Lens' (Button.Opts Action) (Maybe Action))
                    .~ Just closed
                )
            ]
        ]
    ]

selectTypeWidget :: Model -> ATraversal' Model (Field a Unique) -> View Action
selectTypeWidget st optic =
  Grid.bigCell
    [ Select.select
        ( Select.defOpts @Action
            & #optsLabel
            .~ Just @Text "Type"
            & (#optsSelected :: Lens' (Select.Opts Action) (Maybe Select.Item))
            .~ ( fmap newItem
                  $ st
                  ^? cloneTraversal optic
                  . #fieldType
               )
            & #optsOnChange
            .~ Just
              ( \x ->
                  pureUpdate
                    0
                    ( &
                        cloneTraversal optic
                          . #fieldType
                          .~ toEnum
                            ( Prelude.read
                                $ from @Text @String x
                            )
                    )
              )
            & (#optsItems :: Lens' (Select.Opts Action) [Select.Item])
            .~ (
                 --
                 -- NOTE : maybe add icons:
                 --
                 -- "font_download"
                 -- "qr_code_2"
                 -- "link"
                 -- "code"
                 --
                 fmap newItem
                  $ enumerate @FieldType
               )
        )
    ]
  where
    newItem t =
      Select.Item
        { Select.itemLabel = userFieldType t,
          Select.itemLeadingIcon = Nothing,
          --
          -- TODO : implement
          --
          Select.itemValue = userFieldType t
        }

constTextField :: Model -> Text -> Opts -> View Action
constTextField st txt opts =
  Grid.mediumCell
    [ div_
        ( catMaybes
            [ Just $ class_ "control",
              if isJust (opts ^. #optsLeadingWidget)
                then Just $ class_ "has-icons-left"
                else Nothing,
              if isJust (opts ^. #optsTrailingWidget)
                then Just $ class_ "has-icons-right"
                else Nothing
            ]
            <> ( if opts ^. #optsFullWidth
                  then [class_ "fill"]
                  else mempty
               )
            <> ( opts ^. #optsExtraAttributes
               )
        )
        $ catMaybes
          [ Just
              $ input_
                [ disabled_ True,
                  value_ $ ms txt,
                  type_ . ms $ htmlFieldType FieldTypeText,
                  placeholder_ . ms $ opts ^. #optsPlaceholder
                ],
            fmap
              ( fieldIconSimple Leading FaCopy mempty . \case
                  CopyWidget -> Misc.copyIntoClipboardAction st txt
                  _ -> error "constTextField unsupported widget"
              )
              ( opts ^. #optsLeadingWidget
              )
          ]
    ]

constLinkField :: Model -> URI -> Opts -> View Action
constLinkField st =
  constTextField st
    . URI.render

--
-- TODO : support optional copying widgets
--
dynamicFieldViewer :: Model -> Field DynamicField Unique -> [View Action]
dynamicFieldViewer st value =
  case value ^. #fieldType of
    FieldTypeNumber -> plain out text
    FieldTypePercent -> plain out $ text . (<> "%")
    FieldTypeText -> plain out text
    FieldTypeTitle -> header out
    FieldTypeQrCode -> Qr.qr st out $ Qr.defOpts & #optsAllowCopy .~ allowCopy
    FieldTypeHtml -> plain out rawHtml
    FieldTypePassword -> plain out $ const "*****"
  where
    out = inspectDynamicField $ value ^. #fieldOutput
    allowCopy = value ^. #fieldAllowCopy

plain ::
  ( Container a,
    ToMisoString a
  ) =>
  a ->
  (MisoString -> View action) ->
  [View action]
plain out widget =
  if null out
    then mempty
    else
      [ span_
          [ class_ "fill",
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
          [widget $ ms out]
      ]

header :: Text -> [View Action]
header txt =
  if null txt
    then mempty
    else
      [ h5_
          [ class_ "fill",
            style_ [("text-align", "center")]
          ]
          [ text $ ms txt
          ]
      ]
