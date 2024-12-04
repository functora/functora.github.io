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

data Request (bos :: BuyOrSell) = Request
  { baseAmount :: MoneyAmount,
    symbol :: CurrencyPair,
    rate :: QuotePerBase,
    options :: Options bos
  }
  deriving stock (Eq, Ord, Show)

data Options (bos :: BuyOrSell) = Options
  { stopLoss :: Maybe QuotePerBase,
    clientId :: Maybe OrderClientId,
    groupId :: Maybe OrderGroupId,
    flags :: Set OrderFlag
  }
  deriving stock (Eq, Ord, Show)

optsDef :: Options bos
optsDef =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      flags = mempty
    }

optsPostOnly :: Options bos
optsPostOnly =
  Options
    { stopLoss = Nothing,
      clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly]
    }

optsPostOnlyStopLoss :: QuotePerBase -> Options bos
optsPostOnlyStopLoss sl =
  Options
    { stopLoss = Just sl,
      clientId = Nothing,
      groupId = Nothing,
      flags = [PostOnly, Oco]
    }

instance
  forall (bos :: BuyOrSell).
  ( Typeable bos,
    SingI bos
  ) =>
  ToJSON (Request bos)
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
            A..= toTextParam
              ( demote @bos,
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
