{-# LANGUAGE TemplateHaskell #-}

module Functora.MoneyFgpt () where

import Functora.MoneySing
import Functora.Tags

mkFgpt @BuyOrSell
mkFgpt @NetOrGross
mkFgpt @GainOrLose
mkFgpt @BaseOrQuote
mkFgpt @QuotePerBase
mkFgpt @FeeRate
mkFgpt @ProfitRate
mkFgpt @SignedOrUnsigned
mkFgpt @CurrencyKind
mkFgpt @MakerOrTaker
mkFgpt @LocalOrRemote
mkFgpt @MinOrMax
mkFgpt @Revenue
