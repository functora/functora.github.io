module App.Widgets.Cell
  ( grid,
    bigCell,
    mediumCell,
    smallCell,
  )
where

import Functora.Prelude
import Miso hiding (at, view)

grid :: [Attribute action] -> [View action] -> View action
grid attrs xs =
  div_
    [ class_ "fixed-grid",
      class_ "has-6-cols-mobile",
      class_ "has-12-cols-tablet",
      class_ "has-12-cols-desktop",
      class_ "has-12-cols-widescreen",
      class_ "has-12-cols-fullhd"
    ]
    [ div_ ([class_ "grid"] <> attrs) xs
    ]

bigCell :: [View action] -> View action
bigCell =
  div_
    [ class_ "cell",
      class_ "is-col-span-12",
      style_ [("align-content", "center")]
    ]

mediumCell :: [View action] -> View action
mediumCell =
  div_
    [ class_ "cell",
      class_ "is-col-span-6",
      style_ [("align-content", "center")]
      -- style_
      --   [ ("display", "flex"),
      --     ("align-items", "center")
      --   ]
    ]

smallCell :: [View action] -> View action
smallCell =
  div_
    [ class_ "cell",
      class_ "is-col-span-3",
      style_ [("align-content", "center")]
    ]
