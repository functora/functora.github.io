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
grid attrs =
  div_ $ [class_ "grid"] <> attrs

bigCell :: [View action] -> View action
bigCell =
  div_
    [ class_ "cell",
      style_ [("align-content", "center")]
    ]

mediumCell :: [View action] -> View action
mediumCell =
  div_
    [ class_ "cell",
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
      style_ [("align-content", "center")]
    ]
