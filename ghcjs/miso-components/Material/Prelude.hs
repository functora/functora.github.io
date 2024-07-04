module Material.Prelude
  ( module Prelude,
    String,
    Text,
  )
where

import qualified Miso.String
import Prelude hiding (String)

type String = Miso.String.MisoString

type Text = Miso.String.MisoString
