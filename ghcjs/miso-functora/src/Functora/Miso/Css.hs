module Functora.Miso.Css
  ( fullWidth,
  )
where

import Functora.Miso.Prelude

fullWidth :: Attribute action
fullWidth = style_ [("width", "100%")]
