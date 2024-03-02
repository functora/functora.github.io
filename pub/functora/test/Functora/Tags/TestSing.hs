{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Functora.Tags.TestSing where

import Functora.Tags

singletons
  [d|
    data Money = Money

    data NetOrGross = Net | Gross

    data GainOrLose = Gain | Lose

    data MerchantOrCustomer = Merchant | Customer
    |]

mkEnum ''Money
mkEnum ''NetOrGross
mkEnum ''GainOrLose
mkEnum ''MerchantOrCustomer
