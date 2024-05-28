module App.Widgets.EditorSettings
  ( editorSettings,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Switch as Switch
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
      $ Switch.switch
        st
        ( Switch.defOpts
            & #optsIcon
            .~ Just "edit"
            & #optsPlaceholder
            .~ "Allow editing"
        )
        ( #modelState
            . #stDoc
            . #stDocEditable
        ),
    Cell.bigCell
      $ Field.constLinkField
        st
        ( either impureThrow id
            $ Misc.appUri st
        )
        ( Field.defOpts
            & #optsPlaceholder
            .~ "QR code link"
        )
  ]
