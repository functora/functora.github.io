{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.SubmitOrder
  ( Request (..),
    Options (..),
    optsDef,
    optsPostOnly,
    optsPostOnlyStopLoss,
  )
where

import Bfx.Class.ToRequestParam
import Bfx.Data.Type
import qualified Data.Aeson as A
import Functora.Cfg
import Functora.Money
import Functora.Prelude

data Request = Request
  { buyOrSell :: BuyOrSell,
    baseAmount :: MoneyAmount,
    symbol :: CurrencyPair,
    rate :: QuotePerBase,
    options :: Options
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data Options = Options
  { stopLoss :: Maybe QuotePerBase,
    clientId :: Maybe OrderClientId,
    groupId :: Maybe OrderGroupId,
    affCode :: Maybe AffCode,
    flags :: Set OrderFlag
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

optsDef :: Options
optsDef =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      affCode = Nothing,
      flags = mempty
    }

optsPostOnly :: Options
optsPostOnly =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      affCode = Nothing,
      flags = [PostOnly]
    }

optsPostOnlyStopLoss :: QuotePerBase -> Options
optsPostOnlyStopLoss sl =
  Options
    { stopLoss = Just sl,
      clientId = Nothing,
      groupId = Nothing,
      affCode = Nothing,
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
      <> maybe
        mempty
        (\x -> ["meta" A..= A.object ["aff_code" A..= unAffCode x]])
        (affCode opts)
    where
      opts = options req
