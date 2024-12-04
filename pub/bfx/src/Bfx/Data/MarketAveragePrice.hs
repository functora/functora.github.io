{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.MarketAveragePrice
  ( Request (..),
  )
where

import Bfx.Import

data Request (bos :: BuyOrSell) = Request
  { baseAmount :: MoneyAmount,
    symbol :: CurrencyPair
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
