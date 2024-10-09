module Functora.Miso.Widgets.FixedOverlay
  ( fixedOverlay,
  )
where

import Functora.Miso.Prelude

fixedOverlay :: [Attribute action] -> [View action] -> View action
fixedOverlay attrs content =
  div_
    [ style_
        [ ("position", "fixed"),
          ("margin", "0"),
          ("padding", "0"),
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
    ]
    $ ( div_
          ( style_
              [ ("position", "fixed"),
                ("top", "0"),
                ("left", "0"),
                ("width", "100%"),
                ("height", "100%")
              ]
              : attrs
          )
          mempty
      )
    : content
