{-# LANGUAGE TemplateHaskell #-}

module Functora.MoneyFgpt () where

import Functora.MoneySing
import Functora.Tags

mkFgpt @MoneyKind
mkFgpt @CurrencyKind
mkFgpt @BuyOrSell
mkFgpt @NetOrGross
mkFgpt @GainOrLose
mkFgpt @BaseOrQuote
mkFgpt @MakerOrTaker
mkFgpt @LocalOrRemote
mkFgpt @MinOrMax
mkFgpt @Revenue
mkFgpt @SignedOrUnsigned
