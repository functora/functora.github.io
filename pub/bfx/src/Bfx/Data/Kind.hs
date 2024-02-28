{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Kind where

import Bfx.Import.External hiding (type (==))
import Data.Singletons.Base.TH

$( singletons
    [d|
      data Method
        = PlatformStatus
        | SymbolsDetails
        | MarketAveragePrice
        | FeeSummary
        | Wallets
        | SubmitOrder
        | RetrieveOrders
        | OrdersHistory
        | CancelOrderMulti
        | CandlesLast
        | CandlesHist
        | Tickers
        deriving stock
          ( Eq,
            Ord,
            Show
          )

      data CurrencyKind
        = Crypto
        | Stable
        | Fiat
        deriving stock
          ( Eq,
            Ord,
            Show
          )

      data CurrencyRelation
        = Base
        | Quote
        deriving stock
          ( Eq,
            Ord,
            Show
          )

      data MarketRelation
        = Maker
        | Taker
        deriving stock
          ( Eq,
            Ord,
            Show
          )

      data Location
        = Local
        | Remote
        deriving stock
          ( Eq,
            Ord,
            Show
          )

      data ExchangeAction
        = Buy
        | Sell
        deriving stock
          ( Eq,
            Ord,
            Show
          )

      data Boundary
        = Min
        | Max
        deriving stock
          ( Eq,
            Ord,
            Show
          )
      |]
 )

deriving stock instance Generic Method

deriving stock instance Generic CurrencyKind

deriving stock instance Generic CurrencyRelation

deriving stock instance Generic MarketRelation

deriving stock instance Generic Location

deriving stock instance Generic ExchangeAction

deriving stock instance Generic Boundary

instance ToJSON Method

instance ToJSON CurrencyKind

instance ToJSON CurrencyRelation

instance ToJSON MarketRelation

instance ToJSON Location

instance ToJSON ExchangeAction

instance ToJSON Boundary

instance FromJSON Method

instance FromJSON CurrencyKind

instance FromJSON CurrencyRelation

instance FromJSON MarketRelation

instance FromJSON Location

instance FromJSON ExchangeAction

instance FromJSON Boundary
