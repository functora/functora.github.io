module Functora.Miso.Widgets.FixedOverlay
  ( fixedOverlay,
  )
where

import Functora.Miso.Prelude

fixedOverlay :: [View action] -> View action
fixedOverlay =
  div_
    . singleton
    $ style_
      [ ("position", "fixed"),
        ("top", "0"),
        ("left", "0"),
        ("width", "100%"),
        ("height", "100%"),
        ("backdrop-filter", "blur(2px)"),
        ("background-color", "rgba(0, 0, 0, 0.5)"),
        ("z-index", "9999"),
        ("display", "flex"),
        ("justify-content", "center"),
        ("align-items", "center")
      ]
