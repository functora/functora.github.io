module App.Widgets.Field
  ( Opts (..),
    opts,
    rationalField,
    textField,
    dynamicField,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import Functora.Prelude hiding (Field (..), field)
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

data Opts = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: Text,
    optsExtraOnInput :: Model -> Model,
    optsSettingsIcon :: Bool
  }
  deriving stock (Generic)

opts :: Opts
opts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty,
      optsExtraOnInput = id,
      optsSettingsIcon = True
    }

field ::
  Model ->
  ATraversal' Model (Field a Unique) ->
  Opts ->
  (Field a Unique -> Maybe a) ->
  (a -> Text) ->
  View Action
field st optic options parser viewer =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ TextField.filled
        $ TextField.config
        & TextField.setType
          ( from @Text @String
              <$> st
              ^? cloneTraversal optic
              . #fieldHtmlType
          )
        & TextField.setOnInput onInputAction
        & TextField.setDisabled (options ^. #optsDisabled)
        & TextField.setLabel
          ( Just . from @Text @String $ options ^. #optsPlaceholder
          )
        & TextField.setLeadingIcon
          ( Just
              $ if options ^. #optsSettingsIcon
                then
                  TextField.icon
                    [ class_ "mdc-text-field__icon--leading",
                      style_ [("pointer-events", "auto")],
                      textProp "role" "button",
                      intProp "tabindex" 0,
                      onClick opened
                    ]
                    "settings"
                else
                  TextField.icon
                    [ class_ "mdc-text-field__icon--leading",
                      style_ [("pointer-events", "auto")],
                      textProp "role" "button",
                      intProp "tabindex" 0,
                      onClick onCopyAction
                    ]
                    "content_copy"
          )
        & TextField.setTrailingIcon
          ( if options ^. #optsDisabled
              then Nothing
              else
                Just
                  $ TextField.icon
                    [ class_ "mdc-text-field__icon--trailing",
                      intProp "tabindex" 0,
                      textProp "role" "button",
                      onClick onClearAction
                    ]
                    "close"
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_ . ms $ htmlUid @Text uid,
            onKeyDown $ Misc.onKeyDownAction uid,
            onBlur onBlurAction
          ],
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( fromMaybe
                  False
                  (st ^? cloneTraversal optic . #fieldSettingsOpen)
              )
        )
        ( Dialog.dialogContent
            Nothing
            [ Cell.grid
                mempty
                [ Cell.mediumCell
                    $ Select.outlined
                      ( Select.config
                          & Select.setLabel (Just "Show as")
                          & Select.setAttributes [class_ "fill-inner"]
                          & Select.setSelected
                            ( st ^? cloneTraversal optic . #fieldFormat
                            )
                          & Select.setOnChange
                            ( \x ->
                                pureUpdate
                                  0
                                  ( & cloneTraversal optic . #fieldFormat .~ x
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
                          (SelectItem.config FieldFormatText)
                          [text "Text"]
                      )
                      [ SelectItem.selectItem
                          (SelectItem.config FieldFormatQrCode)
                          [text "QR code"],
                        SelectItem.selectItem
                          (SelectItem.config FieldFormatLink)
                          [text "Link"],
                        SelectItem.selectItem
                          (SelectItem.config FieldFormatHtml)
                          [text "HTML"]
                      ],
                  Cell.smallCell
                    $ Button.raised
                      ( Button.config
                          & Button.setOnClick closed
                          & Button.setIcon (Just "keyboard_double_arrow_down")
                          & Button.setAttributes
                            [ class_ "fill",
                              Theme.secondaryBg
                            ]
                      )
                      "Down",
                  Cell.smallCell
                    $ Button.raised
                      ( Button.config
                          & Button.setOnClick closed
                          & Button.setIcon (Just "keyboard_double_arrow_up")
                          & Button.setAttributes
                            [ class_ "fill",
                              Theme.secondaryBg
                            ]
                      )
                      "Up",
                  Cell.smallCell
                    $ Button.raised
                      ( Button.config
                          & Button.setOnClick closed
                          & Button.setIcon (Just "library_add")
                          & Button.setAttributes
                            [ class_ "fill",
                              Theme.secondaryBg
                            ]
                      )
                      "Clone",
                  Cell.smallCell
                    $ Button.raised
                      ( Button.config
                          & Button.setOnClick closed
                          & Button.setIcon (Just "delete_forever")
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
                          & Button.setAttributes
                            [ class_ "fill"
                            ]
                      )
                      "Cancel"
                ]
            ]
            mempty
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
                  & (options ^. #optsExtraOnInput)
           in next
                & cloneTraversal optic
                . #fieldOutput
                %~ maybe id (const . id) (getOutput next)
    onCopyAction =
      PushUpdate $ do
        Misc.verifyUid uid
        whenJust (getInput st) $ Misc.copyIntoClipboard st
        pure $ ChanItem 0 id
    onClearAction =
      PushUpdate $ do
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
            & (options ^. #optsExtraOnInput)
    opened =
      pureUpdate 0 $ \st' ->
        st'
          & cloneTraversal optic
          . #fieldSettingsOpen
          .~ True
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneTraversal optic
          . #fieldSettingsOpen
          .~ False

rationalField ::
  Model ->
  ATraversal' Model (Field Rational Unique) ->
  Opts ->
  View Action
rationalField st optic options =
  field
    st
    optic
    ( options & #optsSettingsIcon .~ False
    )
    ( parseRatio . view (#fieldInput . #uniqueValue)
    )
    inspectRatioDef

textField ::
  Model ->
  ATraversal' Model (Field Text Unique) ->
  Opts ->
  View Action
textField st optic options =
  field
    st
    optic
    ( options & #optsSettingsIcon .~ False
    )
    ( Just . view (#fieldInput . #uniqueValue)
    )
    id

dynamicField ::
  Model ->
  ATraversal' Model (Field FieldOutput Unique) ->
  Opts ->
  View Action
dynamicField st optic options =
  field
    st
    optic
    options
    parseFieldOutput
    inspectFieldOutput
