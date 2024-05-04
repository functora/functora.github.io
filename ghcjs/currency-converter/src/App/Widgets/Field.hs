module App.Widgets.Field
  ( Opts (..),
    opts,
    field,
  )
where

import qualified App.Misc as Misc
import App.Types
import Functora.Prelude hiding (Field (..), field)
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.TextField as TextField
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

data Opts = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: Text,
    optsExtraOnInput :: Model -> Model,
    optsLeadingSettings :: Bool
  }
  deriving stock (Generic)

opts :: Opts
opts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty,
      optsExtraOnInput = id,
      optsLeadingSettings = True
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
              $ if options ^. #optsLeadingSettings
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
            [ -- currencyListWidget st optic
              text "HELLO"
            ]
            [ div_
                [ class_ "fill"
                ]
                [ -- TextInput.textInput
                  --   st
                  --   ( cloneTraversal optic
                  --       . #currencyInput
                  --   )
                  --   ( TextInput.opts
                  --       & #optsPlaceholder
                  --       .~ "Search"
                  --   ),
                  Button.raised
                    ( Button.config
                        & Button.setOnClick closed
                        & Button.setAttributes
                          [ class_ "fill"
                          ]
                    )
                    "Cancel"
                ]
            ]
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
