module Functora.Miso.Capa.Grid
  ( grid,
    bigCell,
    mediumCell,
    smallCell,
    microCell,
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

bigCell :: [View action] -> View action
bigCell =
  LayoutGrid.cell
    [ LayoutGrid.span12Desktop,
      LayoutGrid.span8Tablet,
      LayoutGrid.span4Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]

mediumCell :: [View action] -> View action
mediumCell =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]

smallCell :: [View action] -> View action
smallCell =
  LayoutGrid.cell
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span2Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]

microCell :: [View action] -> View action
microCell =
  LayoutGrid.cell
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
      LayoutGrid.span1Phone,
      LayoutGrid.alignMiddle,
      style_ [("align-content", "center")]
    ]
