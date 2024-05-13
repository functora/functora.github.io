module App.Widgets.Cell
  ( grid,
    bigCell,
    mediumCell,
    smallCell,
  )
where

import Functora.Prelude
import qualified Material.LayoutGrid as LayoutGrid
import Miso hiding (at, view)

grid :: [Attribute action] -> [View action] -> View action
grid attrs cells =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    [ LayoutGrid.inner attrs cells
    ]

bigCell :: View action -> View action
bigCell =
  LayoutGrid.cell
    [ LayoutGrid.span12Desktop,
      LayoutGrid.span8Tablet,
      LayoutGrid.span4Phone
    ]
    . (: mempty)

mediumCell :: View action -> View action
mediumCell =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone
    ]
    . (: mempty)

smallCell :: View action -> View action
smallCell =
  LayoutGrid.cell
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span2Phone
    ]
    . (: mempty)
