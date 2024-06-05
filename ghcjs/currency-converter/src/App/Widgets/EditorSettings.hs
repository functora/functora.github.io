module App.Widgets.EditorSettings
  ( editorSettings,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.TextArea as TextArea
import qualified Material.Theme as Theme
import Miso hiding (view)
import qualified Text.URI as URI

editorSettings :: Model -> [View Action]
editorSettings st =
  Header.headerEditor
    st
    ( #modelState
        . #stPre
    )
    ( Field.defOpts
        & #optsPlaceholder
        .~ ( "Preview - "
              <> ( st
                    ^. #modelState
                    . #stPre
                    . #fieldType
                    . to userFieldType
                 )
           )
        & #optsLeadingWidget
        .~ Just
          ( Field.ModalWidget
              $ Field.ModalMiniWidget (#modelState . #stPre)
          )
    )
    <> [ shareModal st,
         Cell.mediumCell
          $ Field.passwordField st (#modelState . #stIkm) Field.defOpts,
         Cell.smallCell
          $ Button.raised
            ( Button.config
                & Button.setIcon (Just "share")
                & Button.setAttributes [class_ "fill"]
                & Button.setOnClick
                  ( Misc.copyIntoClipboardAction st
                      $ shareLink @Text Viewer st
                  )
            )
            "Link",
         Cell.smallCell
          $ Button.raised
            ( Button.config
                & Button.setIcon (Just "more_horiz")
                & Button.setAttributes [class_ "fill"]
                & Button.setOnClick (pureUpdate 0 (& #modelShare .~ Opened))
            )
            "More"
       ]

shareModal :: Model -> View Action
shareModal st =
  Dialog.dialog
    ( Dialog.config
        & Dialog.setOnClose closed
        & Dialog.setOpen (Opened == st ^. #modelShare)
    )
    ( Dialog.dialogContent
        Nothing
        [ Cell.grid
            mempty
            $ shareWidget st Viewer
            <> shareWidget st Editor
            <> [ Cell.bigCell
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
  where
    closed = pureUpdate 0 (& #modelShare .~ Closed)

shareWidget :: Model -> Screen -> [View Action]
shareWidget st screen =
  [ Cell.bigCell
      . TextArea.filled
      $ TextArea.config
      & TextArea.setValue (Just screenLink)
      & TextArea.setLabel (Just "Link")
      & TextArea.setDisabled True
      & TextArea.setFullwidth True,
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "share")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick (Misc.copyIntoClipboardAction st screenLink)
        )
        ( inspect screen
        ),
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick
              ( PushUpdate $ do
                  uri <- URI.mkURI $ from @String @Text screenLink
                  new <- newModel (Just $ st ^. #modelMarket) uri
                  pure . ChanItem 0 $ const new
              )
        )
        ( inspect screen
        ),
    Cell.bigCell
      . TextArea.filled
      $ TextArea.config
      & TextArea.setValue (Just screenQrCode)
      & TextArea.setLabel (Just $ inspect screen <> " QR code")
      & TextArea.setDisabled True
      & TextArea.setFullwidth True,
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "share")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick (Misc.copyIntoClipboardAction st screenQrCode)
        )
        ( inspect screen <> " QR"
        ),
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick
              ( PushUpdate $ do
                  uri <- URI.mkURI $ from @String @Text screenQrCode
                  new <- newModel (Just $ st ^. #modelMarket) uri
                  pure . ChanItem 0 $ const new
              )
        )
        ( inspect screen <> " QR"
        )
  ]
  where
    screenLink = shareLink screen st
    screenQrCode = shareLink (QrCode screen) st

shareLink :: forall a. (From Text a) => Screen -> Model -> a
shareLink sc =
  from @Text @a
    . either impureThrow URI.render
    . Misc.stUri
    . Misc.setScreenPure sc
