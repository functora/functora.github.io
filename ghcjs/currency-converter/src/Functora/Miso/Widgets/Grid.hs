module Functora.Miso.Widgets.Grid
  ( grid,
    bigCell,
    mediumCell,
    smallCell,
  )
where

import Functora.Miso.Prelude
import qualified Material.LayoutGrid as LayoutGrid

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
      LayoutGrid.span4Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]
    . (: mempty)

mediumCell :: View action -> View action
mediumCell =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]
    . (: mempty)

smallCell :: View action -> View action
smallCell =
  LayoutGrid.cell
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span2Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]
    . (: mempty)
