{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.LogOrphan () where

import Functora.Prelude
import Katip
import qualified Language.Haskell.TH.Syntax as TH

deriving stock instance Data Severity

deriving stock instance TH.Lift TH.Loc
