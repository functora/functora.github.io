{-# LANGUAGE TemplateHaskell #-}

module Functora.Tags.TestFgpt () where

import Functora.Tags
import Functora.Tags.TestSing

mkFgpt @Money
mkFgpt @GainOrLose
mkFgpt @NetOrGross
mkFgpt @MerchantOrCustomer
