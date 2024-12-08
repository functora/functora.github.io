{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Class.ToRequestMethod
  ( ToRequestMethod (..),
  )
where

import Bfx.Data.Kind
import Bfx.Data.Web

class ToRequestMethod (method :: Method) where
  toRequestMethod :: RequestMethod

instance ToRequestMethod 'PlatformStatus where
  toRequestMethod = GET

instance ToRequestMethod 'SymbolsDetails where
  toRequestMethod = GET

instance ToRequestMethod 'MarketAveragePrice where
  toRequestMethod = POST

instance ToRequestMethod 'FeeSummary where
  toRequestMethod = POST

instance ToRequestMethod 'SubmitOrder where
  toRequestMethod = POST

instance ToRequestMethod 'RetrieveOrders where
  toRequestMethod = POST

instance ToRequestMethod 'OrdersHistory where
  toRequestMethod = POST

instance ToRequestMethod 'CancelOrderMulti where
  toRequestMethod = POST

instance ToRequestMethod 'Wallets where
  toRequestMethod = POST

instance ToRequestMethod 'CandlesLast where
  toRequestMethod = GET

instance ToRequestMethod 'CandlesHist where
  toRequestMethod = GET

instance ToRequestMethod 'Tickers where
  toRequestMethod = GET
