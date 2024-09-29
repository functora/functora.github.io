module Functora.Miso.Widgets.Grid
  ( grid,
    bigCell,
    mediumCell,
    smallCell,
    microCell,
  )
where

import Functora.Miso.Prelude

grid :: [Attribute action] -> [View action] -> View action
grid attrs =
  div_
    $ style_
      [ ("gap", "1rem"),
        ("display", "grid"),
        ("margin", "0 auto"),
        ("grid-template-columns", "repeat(auto-fit, minmax(300px, 1fr))")
      ]
    : attrs

cell :: [View action] -> View action
cell =
  div_
    $ [ style_
          [ ("height", "4rem"),
            ("padding", "1rem")
          ]
      ]

bigCell :: [View action] -> View action
bigCell = cell

mediumCell :: [View action] -> View action
mediumCell = cell

smallCell :: [View action] -> View action
smallCell = cell

microCell :: [View action] -> View action
microCell = cell
