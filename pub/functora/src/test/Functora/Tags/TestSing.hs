{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.Tags.TestSing where

import Functora.Tags

data Money = Money

data NetOrGross = Net | Gross

data GainOrLose = Gain | Lose

data MerchantOrCustomer = Merchant | Customer

mkSing ''Money
mkSing ''NetOrGross
mkSing ''GainOrLose
mkSing ''MerchantOrCustomer
