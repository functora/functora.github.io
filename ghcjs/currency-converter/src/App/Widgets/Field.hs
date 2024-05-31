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
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Switch as Switch
import Data.Maybe (listToMaybe)
import Functora.Prelude hiding (Field (..), field)
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import Miso hiding (URI, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Text.URI as URI

data Opts = Opts
  { optsDisabled :: Bool,
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
  | ModalWidget ModalWidget'

data ModalWidget' where
  ModalItemWidget ::
    forall a.
    ( Data a
    ) =>
    ATraversal' Model [a] ->
    Int ->
    ATraversal' a [FieldPair DynamicField Unique] ->
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
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
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
    <> [ TextField.filled
          $ TextField.config
          & TextField.setType
            ( fmap
                (from @Text @String . htmlFieldType)
                (st ^? cloneTraversal optic . #fieldType)
            )
          & TextField.setOnInput onInputAction
          & TextField.setDisabled (opts ^. #optsDisabled)
          & TextField.setLabel
            ( Just . from @Text @String $ opts ^. #optsPlaceholder
            )
          & TextField.setLeadingIcon
            ( fmap
                (fieldIcon st optic Leading $ opts ^. #optsExtraOnInput)
                (opts ^. #optsLeadingWidget)
            )
          & TextField.setTrailingIcon
            ( fmap
                (fieldIcon st optic Trailing $ opts ^. #optsExtraOnInput)
                (opts ^. #optsTrailingWidget)
            )
          & TextField.setAttributes
            ( [ class_ "fill",
                id_ . ms $ htmlUid @Text uid,
                onKeyDown . optsOnKeyDownAction opts $ uid,
                onBlur onBlurAction
              ]
                <> ( opts ^. #optsExtraAttributes
                   )
            )
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
                  .~ from @String @Text txt
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
  TextField.Icon Action
fieldIcon st optic lot extraOnInput = \case
  CopyWidget ->
    fieldIconSimple lot "content_copy" mempty . PushUpdate $ do
      Misc.verifyUid uid
      whenJust (st ^? cloneTraversal optic . #fieldInput . #uniqueValue)
        $ Misc.copyIntoClipboard st
      pure
        $ ChanItem 0 id
  ClearWidget ->
    fieldIconSimple lot "close" mempty . PushUpdate $ do
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
        fieldIconSimple lot "visibility_off" mempty
          $ pureUpdate 0 (& cloneTraversal optic . #fieldType .~ FieldTypeText)
      _ ->
        fieldIconSimple lot "visibility" mempty
          $ pureUpdate 0 (& cloneTraversal optic . #fieldType .~ FieldTypePassword)
  UpWidget opt idx attrs ->
    fieldIconSimple lot "keyboard_double_arrow_up" attrs
      $ Misc.moveUp opt idx
  DownWidget opt idx attrs ->
    fieldIconSimple lot "keyboard_double_arrow_down" attrs
      $ Misc.moveDown opt idx
  DeleteWidget opt idx attrs ->
    fieldIconSimple lot "delete_forever" attrs
      $ Misc.removeAt opt idx
  ModalWidget (ModalItemWidget opt idx _ ooc) ->
    fieldIconSimple lot "settings" [Theme.primary]
      $ pureUpdate
        0
        ( &
            cloneTraversal opt
              . ix idx
              . cloneTraversal ooc
              .~ Opened
        )
  ModalWidget (ModalFieldWidget opt idx access _) ->
    fieldIconSimple lot "settings" mempty
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
    fieldIconSimple lot "settings" mempty
      $ pureUpdate
        0
        ( &
            cloneTraversal opt
              . #fieldModalState
              .~ Opened
        )
  where
    uid =
      fromMaybe nilUid $ st ^? cloneTraversal optic . #fieldInput . #uniqueUid

fieldIconSimple ::
  LeadingOrTrailing ->
  String ->
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

fieldModal ::
  Model ->
  ModalWidget' ->
  View Action
fieldModal st (ModalItemWidget opt idx fps ooc) = do
  let closed =
        pureUpdate
          0
          ( &
              cloneTraversal opt
                . ix idx
                . cloneTraversal ooc
                .~ Closed
          )
  Dialog.dialog
    ( Dialog.config
        & Dialog.setOnClose closed
        & Dialog.setOpen
          ( Just Opened
              == ( st
                    ^? cloneTraversal opt
                    . ix idx
                    . cloneTraversal ooc
                 )
          )
    )
    ( Dialog.dialogContent
        Nothing
        [ Cell.grid
            mempty
            [ Cell.bigCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( Misc.newFieldPairAction
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
                  "Add details",
              Cell.smallCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( Misc.moveDown opt idx
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
              Cell.smallCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( Misc.moveUp opt idx
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
              Cell.smallCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( Misc.duplicateAt opt idx
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
              Cell.smallCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick
                        ( Misc.removeAt opt idx
                        )
                      & Button.setIcon
                        ( Just "delete_forever"
                        )
                      & Button.setAttributes
                        [ class_ "fill",
                          Theme.secondaryBg
                        ]
                  )
                  "Delete",
              Cell.bigCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick closed
                      & Button.setIcon (Just "arrow_back")
                      & Button.setAttributes [class_ "fill"]
                  )
                  "Back"
            ]
        ]
        mempty
    )
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
  Dialog.dialog
    ( Dialog.config
        & Dialog.setOnClose closed
        & Dialog.setOpen
          ( Just Opened
              == (st ^? cloneTraversal optic . #fieldModalState)
          )
    )
    ( Dialog.dialogContent
        Nothing
        [ Cell.grid
            mempty
            $ ( case sod of
                  Static -> mempty
                  Dynamic ->
                    [ let typ :| typs = enumerateNE @FieldType
                       in Cell.mediumCell
                            $ Select.outlined
                              ( Select.config
                                  & Select.setLabel
                                    ( Just "Type"
                                    )
                                  & Select.setAttributes
                                    [ class_ "fill-inner"
                                    ]
                                  & Select.setSelected
                                    ( st
                                        ^? cloneTraversal optic
                                        . #fieldType
                                    )
                                  & Select.setOnChange
                                    ( \x ->
                                        pureUpdate
                                          0
                                          ( &
                                              cloneTraversal optic
                                                . #fieldType
                                                .~ x
                                          )
                                    )
                              )
                              --
                              -- NOTE : maybe add icons:
                              --
                              -- "font_download"
                              -- "qr_code_2"
                              -- "link"
                              -- "code"
                              --
                              ( SelectItem.selectItem
                                  (SelectItem.config typ)
                                  [text . ms $ userFieldType typ]
                              )
                            $ fmap
                              ( \t ->
                                  SelectItem.selectItem
                                    (SelectItem.config t)
                                    [text . ms $ userFieldType t]
                              )
                              typs
                    ]
              )
            <> [ Cell.mediumCell
                  $ Switch.switch
                    st
                    ( Switch.defOpts
                        & #optsIcon
                        .~ Just "content_copy"
                        & #optsPlaceholder
                        .~ "Allow copy"
                    )
                    ( cloneTraversal optic
                        . #fieldAllowCopy
                    ),
                 Cell.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( Misc.moveDown opt idx
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
                 Cell.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( Misc.moveUp opt idx
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
                 Cell.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( Misc.duplicateAt opt idx
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
                 Cell.smallCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick
                          ( Misc.removeAt opt idx
                          )
                        & Button.setIcon
                          ( Just "delete_forever"
                          )
                        & Button.setAttributes
                          [ class_ "fill",
                            Theme.secondaryBg
                          ]
                    )
                    "Delete",
                 Cell.bigCell
                  $ Button.raised
                    ( Button.config
                        & Button.setOnClick closed
                        & Button.setIcon (Just "arrow_back")
                        & Button.setAttributes [class_ "fill"]
                    )
                    "Back"
               ]
        ]
        mempty
    )
fieldModal st (ModalMiniWidget opt) = do
  let closed =
        pureUpdate 0 (& cloneTraversal opt . #fieldModalState .~ Closed)
  Dialog.dialog
    ( Dialog.config
        & Dialog.setOnClose closed
        & Dialog.setOpen
          ( Just Opened == st ^? cloneTraversal opt . #fieldModalState
          )
    )
    ( Dialog.dialogContent
        Nothing
        [ Cell.grid
            mempty
            [ Cell.bigCell
                $ selectTypeWidget st opt,
              Cell.bigCell
                $ Button.raised
                  ( Button.config
                      & Button.setOnClick closed
                      & Button.setIcon (Just "arrow_back")
                      & Button.setAttributes [class_ "fill"]
                  )
                  "Back"
            ]
        ]
        mempty
    )

selectTypeWidget :: Model -> ATraversal' Model (Field a Unique) -> View Action
selectTypeWidget st optic =
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
              ( st
                  ^? cloneTraversal optic
                  . #fieldType
              )
            & Select.setOnChange
              ( \x ->
                  pureUpdate
                    0
                    ( &
                        cloneTraversal optic
                          . #fieldType
                          .~ x
                    )
              )
        )
        --
        -- NOTE : maybe add icons:
        --
        -- "font_download"
        -- "qr_code_2"
        -- "link"
        -- "code"
        --
        ( SelectItem.selectItem
            (SelectItem.config typ)
            [text . ms $ userFieldType typ]
        )
        $ fmap
          ( \t ->
              SelectItem.selectItem
                (SelectItem.config t)
                [text . ms $ userFieldType t]
          )
          typs

constTextField :: Model -> Text -> Opts -> View Action
constTextField st txt opts =
  TextField.filled
    $ TextField.config
    & TextField.setType (Just . from @Text @String $ htmlFieldType FieldTypeLink)
    & TextField.setLabel (Just . from @Text @String $ opts ^. #optsPlaceholder)
    & TextField.setDisabled True
    & TextField.setLeadingIcon
      ( Just
          . fieldIconSimple Leading "content_copy" [Theme.primary]
          . PushUpdate
          $ do
            Misc.copyIntoClipboard st txt
            pure $ ChanItem 0 id
      )
    --
    -- TODO : change state in current app instance!
    --
    -- & TextField.setTrailingIcon
    --   ( fmap
    --       (fieldIcon st optic Trailing $ opts ^. #optsExtraOnInput)
    --       (opts ^. #optsTrailingWidget)
    --   )
    & TextField.setValue (Just $ from @Text @String txt)
    & TextField.setAttributes [class_ "fill"]

constLinkField :: Model -> URI -> Opts -> View Action
constLinkField st =
  constTextField st
    . URI.render
