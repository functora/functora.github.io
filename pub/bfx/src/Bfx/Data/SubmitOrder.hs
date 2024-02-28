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

data Request (act :: ExchangeAction) = Request
  { amount :: Money 'Base act,
    symbol :: CurrencyPair,
    rate :: QuotePerBase act,
    options :: Options act
  }
  deriving stock (Eq, Ord, Show)

data Options (act :: ExchangeAction) = Options
  { stopLoss :: Maybe (QuotePerBase act),
    clientId :: Maybe OrderClientId,
    groupId :: Maybe OrderGroupId,
    flags :: Set OrderFlag
  }
  deriving stock (Eq, Ord, Show)

optsDef :: Options act
optsDef =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      flags = mempty
    }

optsPostOnly :: Options act
optsPostOnly =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly]
    }

optsPostOnlyStopLoss :: QuotePerBase act -> Options act
optsPostOnlyStopLoss sl =
  Options
    { stopLoss = Just sl,
      clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly, Oco]
    }

instance
  ( ToRequestParam (Money 'Base act),
    ToRequestParam (QuotePerBase act),
    SingI act
  ) =>
  ToJSON (Request act)
  where
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
            A..= toTextParam (amount req),
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
