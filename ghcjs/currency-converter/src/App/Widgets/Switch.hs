module App.Widgets.Switch
  ( switch,
  )
where

import App.Types
import Functora.Prelude as Prelude
import qualified Material.DataTable as DataTable
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Switch as Switch
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

switch :: Model -> Text -> ATraversal' Model Bool -> View Action
switch st txt optic =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
    ]
    [ DataTable.dataTable
        ( DataTable.config
            & DataTable.setAttributes [class_ "fill"]
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
                [ Miso.text $ ms txt,
                  Miso.text " ",
                  Switch.switch
                    $ Switch.config
                    & Switch.setChecked
                      ( fromMaybe False $ st ^? cloneTraversal optic
                      )
                    & Switch.setOnChange
                      ( pureUpdate 0 (& cloneTraversal optic %~ not)
                      )
                ]
            ]
        ]
        mempty
    ]
