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
import Miso hiding (view)

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
      $ Field.constLinkField
        st
        ( either impureThrow id
            . Misc.appUri
            $ st
            & #modelState
            . #stScreen
            .~ QrViewer
        )
        ( Field.defOpts
            & #optsPlaceholder
            .~ "QR code link"
        ),
    Cell.bigCell
      $ Field.constLinkField
        st
        ( either impureThrow id
            . Misc.appUri
            $ st
            & #modelState
            . #stScreen
            .~ Editor
        )
        ( Field.defOpts
            & #optsPlaceholder
            .~ "Editor link"
        )
  ]
