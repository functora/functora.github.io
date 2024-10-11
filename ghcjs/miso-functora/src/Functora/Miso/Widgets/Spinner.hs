module Functora.Miso.Widgets.Spinner
  ( spinner,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Widgets.FixedOverlay as FixedOverlay

spinner :: [View action]
spinner =
  FixedOverlay.fixedOverlay
    mempty
    [ div_
        [ class_ "lds-dual-ring",
          style_
            [ ("position", "fixed"),
              ("top", "50%"),
              ("left", "50%"),
              ("transform", "translate(-50%, -50%)"),
              ("z-index", "9999")
            ]
        ]
        mempty
    ]
