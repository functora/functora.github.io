{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.Tags.TestSing where

import Functora.Tags
import Prelude

data Money = Money
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data NetOrGross = Net | Gross
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data GainOrLose = Gain | Lose
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data MerchantOrCustomer = Merchant | Customer
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

mkSing ''Money
mkSing ''NetOrGross
mkSing ''GainOrLose
mkSing ''MerchantOrCustomer
