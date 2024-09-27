module Functora.Miso.Capa.Frame
  ( frame,
  )
where

import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import qualified Material.DataTable as DataTable
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Typography as Typography

frame :: [View action] -> View action
frame content =
  DataTable.dataTable
    ( DataTable.config
        & DataTable.setAttributes
          [ Css.fullWidth,
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
