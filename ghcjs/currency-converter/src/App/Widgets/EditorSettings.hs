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
import qualified Material.TextArea as TextArea
import qualified Material.Theme as Theme
import Miso hiding (view)
import qualified Text.URI as URI

editorSettings :: Model -> [View Action]
editorSettings st =
  [ Header.header "Share" Nothing,
    Cell.mediumCell
      $ Field.passwordField
        st
        ( #modelState
            . #stIkm
        )
        Field.defOpts,
    Cell.mediumCell
      $ Field.field
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
        parseDynamicField
        inspectDynamicField,
    Cell.bigCell
      . TextArea.filled
      $ TextArea.config
      & TextArea.setValue (Just editorLink)
      & TextArea.setLabel (Just "Editor link")
      & TextArea.setDisabled True
      & TextArea.setFullwidth True,
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "content_copy")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick (Misc.copyIntoClipboardAction st editorLink)
        )
        "Editor link",
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick
              ( PushUpdate $ do
                  uri <- URI.mkURI $ from @String @Text editorLink
                  new <- newModel (Just $ st ^. #modelMarket) uri
                  pure . ChanItem 0 $ const new
              )
        )
        "Editor link",
    Cell.bigCell
      . TextArea.filled
      $ TextArea.config
      & TextArea.setValue (Just editorQrCode)
      & TextArea.setLabel (Just "Editor QR code")
      & TextArea.setDisabled True
      & TextArea.setFullwidth True,
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "content_copy")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick (Misc.copyIntoClipboardAction st editorQrCode)
        )
        "Editor QR code",
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill", Theme.secondaryBg]
            & Button.setOnClick
              ( PushUpdate $ do
                  uri <- URI.mkURI $ from @String @Text editorQrCode
                  new <- newModel (Just $ st ^. #modelMarket) uri
                  pure . ChanItem 0 $ const new
              )
        )
        "Editor QR code"
  ]
  where
    editorLink = shareLink Editor st
    editorQrCode = shareLink (QrCode Editor) st

shareLink :: forall a. (From Text a) => Screen -> Model -> a
shareLink sc =
  from @Text @a
    . either impureThrow URI.render
    . Misc.stUri
    . Misc.setScreenPure sc
