{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.GetOrders
  ( Options (..),
    optsDef,
    optsSym,
    optsIds,
  )
where

import BitfinexClient.Data.Type
import BitfinexClient.Import hiding (currencyPair)

data Options = Options
  { currencyPair :: Maybe CurrencyPair,
    orderIds :: Set OrderId
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

optsDef :: Options
optsDef =
  Options
    { currencyPair = Nothing,
      orderIds = mempty
    }

optsSym :: CurrencyPair -> Options
optsSym sym =
  optsDef
    { currencyPair = Just sym
    }

optsIds :: Set OrderId -> Options
optsIds ids =
  optsDef
    { orderIds = ids
    }

instance ToJSON Options where
  toJSON = toJSON . orderIds
