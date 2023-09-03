{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.CfgOrphan () where

import Functora.Prelude
import qualified Text.URI as URI
import qualified Toml

instance Toml.HasCodec URI where
  hasCodec =
    Toml.textBy URI.render $
      first (from @String @Text . displayException) . URI.mkURI
