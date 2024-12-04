{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.SubmitOrder
  ( Request (..),
    Options (..),
    optsDef,
    optsPostOnly,
    optsPostOnlyStopLoss,
  )
where

import Bfx.Import
import qualified Data.Aeson as A

data Request = Request
  { baseAmount :: MoneyAmount,
    symbol :: CurrencyPair,
    rate :: QuotePerBase,
    options :: Options,
    buyOrSell :: BuyOrSell
  }
  deriving stock (Eq, Ord, Show)

data Options = Options
  { stopLoss :: Maybe QuotePerBase,
    clientId :: Maybe OrderClientId,
    groupId :: Maybe OrderGroupId,
    flags :: Set OrderFlag
  }
  deriving stock (Eq, Ord, Show)

optsDef :: Options
optsDef =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      flags = mempty
    }

optsPostOnly :: Options
optsPostOnly =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly]
    }

optsPostOnlyStopLoss :: QuotePerBase -> Options
optsPostOnlyStopLoss sl =
  Options
    { stopLoss = Just sl,
      clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly, Oco]
    }

instance ToJSON Request where
  toJSON req =
    eradicateNull
      . A.object
      $ [ "gid"
            A..= groupId opts,
          "cid"
            A..= clientId opts,
          "type"
            A..= ("EXCHANGE LIMIT" :: Text),
          "amount"
            A..= toTextParam
              ( buyOrSell req,
                Base,
                baseAmount req
              ),
          "symbol"
            A..= toTextParam (symbol req),
          "price"
            A..= toTextParam (rate req),
          "flags"
            A..= unOrderFlagSet (flags opts)
            --
            -- TODO : use TIF for automated cancellation!
            --
        ]
      <> maybe
        mempty
        (\x -> ["price_oco_stop" A..= toTextParam x])
        (stopLoss opts)
    where
      opts = options req
