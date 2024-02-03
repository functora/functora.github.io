{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.MoneyOrphan () where

import Data.Money as D
import Functora.Prelude
import qualified Language.Haskell.TH.Syntax as TH

deriving stock instance (Read a) => Read (D.Money a)

deriving stock instance (Data a) => Data (D.Money a)

deriving stock instance (TH.Lift a) => TH.Lift (D.Money a)
