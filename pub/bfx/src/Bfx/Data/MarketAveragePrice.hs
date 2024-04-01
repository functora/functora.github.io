{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.MarketAveragePrice
  ( Request (..),
  )
where

import Bfx.Import

data Request (bos :: BuyOrSell) = Request
  { amount :: Money (Tags 'Unsigned |+| 'Base |+| bos),
    symbol :: CurrencyPair
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
