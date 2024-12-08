{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Class.ToBaseUrl
  ( ToBaseUrl (..),
  )
where

import Bfx.Data.Kind
import Bfx.Data.Web

class ToBaseUrl (method :: Method) where
  toBaseUrl :: BaseUrl

instance ToBaseUrl 'PlatformStatus where
  toBaseUrl = base

instance ToBaseUrl 'SymbolsDetails where
  toBaseUrl = base

instance ToBaseUrl 'MarketAveragePrice where
  toBaseUrl = base

instance ToBaseUrl 'FeeSummary where
  toBaseUrl = base

instance ToBaseUrl 'SubmitOrder where
  toBaseUrl = base

instance ToBaseUrl 'RetrieveOrders where
  toBaseUrl = base

instance ToBaseUrl 'OrdersHistory where
  toBaseUrl = base

instance ToBaseUrl 'CancelOrderMulti where
  toBaseUrl = base

instance ToBaseUrl 'Wallets where
  toBaseUrl = base

instance ToBaseUrl 'CandlesLast where
  toBaseUrl = basePub

instance ToBaseUrl 'CandlesHist where
  toBaseUrl = basePub

instance ToBaseUrl 'Tickers where
  toBaseUrl = basePub

base :: BaseUrl
base = "https://api.bitfinex.com"

basePub :: BaseUrl
basePub = "https://api-pub.bitfinex.com"
