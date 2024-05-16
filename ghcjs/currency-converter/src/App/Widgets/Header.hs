module App.Widgets.Header
  ( header,
    subHeader,
  )
where

import App.Types
import Functora.Prelude
import qualified Material.Fab as Fab
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String (ms)

header :: Text -> Maybe Action -> View Action
header txt action =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.headline4,
      style_
        [ ("text-align", "center")
        ]
    ]
    $ text (ms txt)
    : maybe
      mempty
      ( \act ->
          [ text " ",
            Fab.fab
              ( Fab.config
                  & Fab.setOnClick act
                  & Fab.setAttributes [Theme.secondaryBg]
              )
              "add"
          ]
      )
      action

subHeader :: Text -> View Action
subHeader txt =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      Typography.headline5,
      style_
        [ ("display", "flex"),
          ("align-items", "center"),
          ("justify-content", "center"),
          ("text-align", "center")
        ]
    ]
    [ Miso.text $ ms txt
    ]
