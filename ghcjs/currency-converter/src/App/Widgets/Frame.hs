module App.Widgets.Frame
  ( frame,
  )
where

import App.Prelude as Prelude
import App.Types
import qualified Material.DataTable as DataTable
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography
import Miso hiding (at, view)

frame :: [View Action] -> View Action
frame content =
  DataTable.dataTable
    ( DataTable.config
        & DataTable.setAttributes
          [ class_ "fill",
            class_ "mdc-text-field--outlined"
          ]
    )
    [ DataTable.row
        mempty
        [ DataTable.cell
            [ Typography.body1,
              LayoutGrid.alignMiddle,
              style_
                [ ("display", "flex"),
                  ("align-items", "center"),
                  ("justify-content", "space-between")
                ]
            ]
            content
        ]
    ]
    mempty
