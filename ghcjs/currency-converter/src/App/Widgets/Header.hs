module App.Widgets.Header
  ( header,
  )
where

import App.Types
import Functora.Prelude
import qualified Material.Fab as Fab
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

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
              )
              "add"
          ]
      )
      action
