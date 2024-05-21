module App.Widgets.EditorSettings
  ( editorSettings,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified App.Widgets.Field as Field
import qualified App.Widgets.Header as Header
import qualified App.Widgets.Switch as Switch
import Functora.Prelude as Prelude
-- import qualified Material.Button as Button
-- import qualified Material.Theme as Theme
import Miso hiding (view)

editorSettings :: Model -> [View Action]
editorSettings st =
  [ Header.header "Settings" Nothing,
    Cell.mediumCell
      $ Field.textField
        st
        ( #modelState
            . #statePwd
        )
        ( Field.defOpts
            & #optsPlaceholder
            .~ "Password"
            & #optsLeadingWidget
            .~ Just Field.ShowOrHideWidget
        ),
    Cell.mediumCell
      $ Switch.switch
        st
        ( Switch.defOpts
            & #optsIcon
            .~ Just "edit"
            & #optsPlaceholder
            .~ "Allow edit"
        )
        ( #modelState
            . #stateEditable
        )
  ]
