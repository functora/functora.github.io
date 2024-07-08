module Material.IconButton.Internal (Config (..)) where

import Material.Prelude as Prelude
import Miso

data Config msg = Config
  { disabled :: Bool,
    label :: Maybe String,
    additionalAttributes :: [Attribute msg],
    onClick :: Maybe msg
  }
