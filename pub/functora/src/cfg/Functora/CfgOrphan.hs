{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.CfgOrphan () where

import Functora.Prelude
import qualified Text.URI as URI
import qualified Toml

instance Toml.HasCodec URI where
  hasCodec =
    Toml.textBy URI.render $
      first (from @String @Text . displayException) . URI.mkURI

instance Toml.HasItemCodec URI where
  hasItemCodec =
    Left
      . Toml.mkAnyValueBiMap
        ( \src -> do
            txt <- Toml.matchText src
            first
              (const . Toml.MatchError Toml.TText $ Toml.AnyValue src)
              $ URI.mkURI txt
        )
      $ Toml.Text . URI.render