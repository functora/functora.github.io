module App.Widgets.Grid
  ( grid,
    bigCell,
    mediumCell,
    smallCell,
  )
where

import Functora.Prelude
import Miso hiding (at, view)

grid :: [Attribute action] -> [View action] -> View action
grid attrs =
  div_
    $ [ class_ "columns",
        class_ "is-multiline",
        class_ "is-mobile"
      ]
    <> attrs

bigCell :: [View action] -> View action
bigCell =
  div_
    [ class_ "column",
      class_ "is-full"
    ]

mediumCell :: [View action] -> View action
mediumCell =
  div_
    [ class_ "column",
      class_ "is-full-mobile",
      class_ "is-half-tablet",
      class_ "is-half-desktop",
      class_ "is-half-widescreen",
      class_ "is-half-fullhd"
    ]

smallCell :: [View action] -> View action
smallCell =
  div_
    [ class_ "column",
      class_ "is-half-mobile",
      class_ "is-one-quarter-tablet",
      class_ "is-one-quarter-desktop",
      class_ "is-one-quarter-widescreen",
      class_ "is-one-quarter-fullhd"
    ]
