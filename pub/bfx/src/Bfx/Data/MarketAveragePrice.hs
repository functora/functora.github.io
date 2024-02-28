{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.MarketAveragePrice
  ( Request (..),
  )
where

import Bfx.Import

data Request (act :: ExchangeAction) = Request
  { amount :: Money 'Base act,
    symbol :: CurrencyPair
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
