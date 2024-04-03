{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.MoneySing where

import Functora.Tags

data MoneyKind = MoneyAmount | Currency | QuotePerBase | FeeRate | ProfitRate

data CurrencyKind = Crypto | Stable | Fiat

data BuyOrSell = Buy | Sell

data NetOrGross = Net | Gross

data GainOrLose = Gain | Lose

data BaseOrQuote = Base | Quote

data MakerOrTaker = Maker | Taker

data LocalOrRemote = Local | Remote

data MinOrMax = Min | Max

data Revenue = Revenue

data SignedOrUnsigned = Signed | Unsigned

mkSing ''MoneyKind
mkSing ''CurrencyKind
mkSing ''BuyOrSell
mkSing ''NetOrGross
mkSing ''GainOrLose
mkSing ''BaseOrQuote
mkSing ''MakerOrTaker
mkSing ''LocalOrRemote
mkSing ''MinOrMax
mkSing ''Revenue
mkSing ''SignedOrUnsigned
