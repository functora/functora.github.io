{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Material.Prelude
  ( module Prelude,
    String,
    Text,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Miso
import qualified Miso.String
import Prelude hiding (String)

type String = Miso.String.MisoString

type Text = Miso.String.MisoString

deriving stock instance Generic (Miso.View action)

deriving stock instance Generic (Miso.Attribute action)

deriving stock instance Generic Miso.NS

deriving stock instance Generic Miso.Key

instance NFData (Miso.View action)

instance NFData (Miso.Attribute action)

instance NFData Miso.NS

instance NFData Miso.Key
