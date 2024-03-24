{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functora.MoneyOrphan () where

import Functora.MoneySing
import Functora.Tags

mkFgpt @NetOrGross
mkFgpt @GainOrLose
mkFgpt @BaseOrQuote
mkFgpt @QuotesPerBase
mkFgpt @FeeRate
mkFgpt @SignedOrUnsigned
