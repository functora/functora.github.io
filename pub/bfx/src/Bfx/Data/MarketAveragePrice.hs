{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.MarketAveragePrice
  ( Request (..),
  )
where

import Bfx.Data.Type
import Functora.Money
import Functora.Prelude

data Request = Request
  { buyOrSell :: BuyOrSell,
    baseAmount :: MoneyAmount,
    symbol :: CurrencyPair
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
      Generic
    )
