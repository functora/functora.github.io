{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.MoneySing where

import Functora.Tags

data BuyOrSell = Buy | Sell

data NetOrGross = Net | Gross

data GainOrLose = Gain | Lose

data BaseOrQuote = Base | Quote

data QuotePerBase = QuotePerBase

data FeeRate = FeeRate

data ProfitRate = ProfitRate

data SignedOrUnsigned = Signed | Unsigned

data CurrencyKind = Crypto | Stable | Fiat

data MakerOrTaker = Maker | Taker

data LocalOrRemote = Local | Remote

data MinOrMax = Min | Max

mkSing ''BuyOrSell
mkSing ''NetOrGross
mkSing ''GainOrLose
mkSing ''BaseOrQuote
mkSing ''QuotePerBase
mkSing ''FeeRate
mkSing ''ProfitRate
mkSing ''SignedOrUnsigned
mkSing ''CurrencyKind
mkSing ''MakerOrTaker
mkSing ''LocalOrRemote
mkSing ''MinOrMax
