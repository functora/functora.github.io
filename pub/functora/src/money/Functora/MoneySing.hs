{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.MoneySing where

import Functora.Tags

data Money = Money

data NetOrGross = Net | Gross

data GainOrLose = Gain | Lose

data BaseOrQuote = Base | Quote

data QuotesPerBase = QuotesPerBase

data FeeRate = FeeRate

mkSing ''Money
mkSing ''NetOrGross
mkSing ''GainOrLose
mkSing ''BaseOrQuote
mkSing ''QuotesPerBase
mkSing ''FeeRate
