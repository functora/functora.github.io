{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.TagsOrphan () where

import qualified Data.Kind
import Functora.TagsFamily (Fgpt)
import qualified GHC.TypeLits

type instance Fgpt Data.Kind.Type = "Data.Kind.Type"

type instance Fgpt GHC.TypeLits.Symbol = "GHC.TypeLits.Symbol"
