{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Kind where

import Functora.Cfg
import Singlethongs

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

mkSing ''Method

instance ToJSON Method

instance FromJSON Method
