module Functora.Miso.Widgets.Spinner
  ( spinner,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Widgets.FixedOverlay as FixedOverlay

spinner :: View action
spinner =
  FixedOverlay.fixedOverlay
    mempty
    [ div_ [class_ "lds-dual-ring"] mempty
    ]
