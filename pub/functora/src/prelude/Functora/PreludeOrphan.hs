{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.PreludeOrphan () where

import qualified Data.Data as Data
import Data.Generics (Data)
import Universum
import Witch.Mini

instance Data SomeException where
  gunfold _ z = z . Data.fromConstr
  toConstr _ = error "TODO : toConstr SomeException"
  dataTypeOf _ = error "TODO : dataTypeOf SomeException"

deriving stock instance (Data a, Data b) => Data (TryFromException a b)
