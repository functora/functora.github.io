{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Class.ToPathPieces
  ( ToPathPieces (..),
  )
where

import Bfx.Class.ToRequestParam
import qualified Bfx.Data.Candles as Candles
import qualified Bfx.Data.GetOrders as GetOrders
import Bfx.Data.Kind
import Bfx.Import.External

class ToPathPieces (method :: Method) req where
  toPathPieces :: req -> [Text]

instance ToPathPieces 'PlatformStatus req where
  toPathPieces =
    const ["v2", "platform", "status"]

instance ToPathPieces 'SymbolsDetails req where
  toPathPieces =
    const ["v1", "symbols_details"]

instance ToPathPieces 'MarketAveragePrice req where
  toPathPieces =
    const ["v2", "calc", "trade", "avg"]

instance ToPathPieces 'FeeSummary req where
  toPathPieces =
    const ["v2", "auth", "r", "summary"]

instance ToPathPieces 'SubmitOrder req where
  toPathPieces =
    const ["v2", "auth", "w", "order", "submit"]

instance ToPathPieces 'RetrieveOrders GetOrders.Options where
  toPathPieces x =
    [ "v2",
      "auth",
      "r",
      "orders"
    ]
      <> maybeToList
        ( toTextParam <$> GetOrders.currencyPair x
        )

instance ToPathPieces 'OrdersHistory GetOrders.Options where
  toPathPieces x =
    [ "v2",
      "auth",
      "r",
      "orders"
    ]
      <> maybeToList
        ( toTextParam <$> GetOrders.currencyPair x
        )
      <> [ "hist"
         ]

instance ToPathPieces 'CancelOrderMulti req where
  toPathPieces =
    const
      [ "v2",
        "auth",
        "w",
        "order",
        "cancel",
        "multi"
      ]

instance ToPathPieces 'Wallets req where
  toPathPieces =
    const
      [ "v2",
        "auth",
        "r",
        "wallets"
      ]

instance ToPathPieces 'CandlesLast Candles.Request where
  toPathPieces =
    candlesPathPieces

instance ToPathPieces 'CandlesHist Candles.Request where
  toPathPieces =
    candlesPathPieces

candlesPathPieces :: Candles.Request -> [Text]
candlesPathPieces x =
  [ "v2",
    "candles",
    params,
    toTextParam $ Candles.section x
  ]
  where
    params =
      "trade:"
        <> toTextParam (Candles.timeFrame x)
        <> ":"
        <> toTextParam (Candles.symbol x)

instance ToPathPieces 'Tickers req where
  toPathPieces =
    const
      [ "v2",
        "tickers"
      ]
