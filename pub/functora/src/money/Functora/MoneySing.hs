{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.MoneySing where

import Functora.Tags

data NetOrGross = Net | Gross

data GainOrLose = Gain | Lose

data BaseOrQuote = Base | Quote

data QuotePerBase = QuotePerBase

data FeeRate = FeeRate

data SignedOrUnsigned = Signed | Unsigned

mkSing ''NetOrGross
mkSing ''GainOrLose
mkSing ''BaseOrQuote
mkSing ''QuotePerBase
mkSing ''FeeRate
mkSing ''SignedOrUnsigned
