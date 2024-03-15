{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.MoneyOrphan () where

import qualified Data.Money as D
import Functora.MoneySing
import Functora.Prelude
import Functora.Tags
import qualified Language.Haskell.TH.Syntax as TH

mkFgpt @NetOrGross
mkFgpt @GainOrLose
mkFgpt @BaseOrQuote
mkFgpt @QuotesPerBase
mkFgpt @FeeRate
mkFgpt @SignedOrUnsigned

deriving stock instance (Read a) => Read (D.Money a)

deriving stock instance (Data a) => Data (D.Money a)

deriving stock instance (TH.Lift a) => TH.Lift (D.Money a)
