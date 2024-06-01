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
            . #stHint
        )
        ( Field.defOpts
            & #optsPlaceholder
            .~ ( "Hint - "
                  <> ( st
                        ^. #modelState
                        . #stHint
                        . #fieldType
                        . to userFieldType
                     )
               )
            & #optsLeadingWidget
            .~ Just
              ( Field.ModalWidget
                  $ Field.ModalMiniWidget (#modelState . #stHint)
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
    Cell.bigCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "content_copy")
            & Button.setAttributes [class_ "fill"]
            & Button.setOnClick
              ( PushUpdate $ do
                  Misc.copyIntoClipboard st editorLink
                  pure $ ChanItem 0 id
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
            & Button.setAttributes [class_ "fill"]
            & Button.setOnClick
              ( PushUpdate $ do
                  Misc.copyIntoClipboard st editorQrCode
                  pure $ ChanItem 0 id
              )
        )
        "Editor QR code",
    Cell.mediumCell
      $ Button.raised
        ( Button.config
            & Button.setIcon (Just "login")
            & Button.setAttributes [class_ "fill"]
            & Button.setOnClick (Misc.setScreenAction $ QrCode Editor)
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
