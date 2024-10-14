module Functora.Miso.Widgets.FixedOverlay
  ( fixedOverlay,
  )
where

import Functora.Miso.Prelude

fixedOverlay :: [Attribute action] -> View action
fixedOverlay attrs =
  div_
    ( style_
        [ ("position", "fixed"),
          ("margin", "0"),
          ("padding", "0"),
          ("top", "0"),
          ("left", "0"),
          ("width", "100%"),
          ("height", "100%"),
          ("z-index", "9999"),
          ("backdrop-filter", "blur(2px)"),
          ("background-color", "rgba(0, 0, 0, 0.5)")
        ]
        : attrs
    )
    mempty
