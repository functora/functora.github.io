{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Wallets
  ( WalletType (..),
    newWalletType,
    Response (..),
  )
where

import Bfx.Data.Kind
import Bfx.Data.Metro
import Bfx.Import.External

data WalletType
  = Exchange
  | Margin
  | Funding
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      Enum,
      Bounded
    )

newWalletType ::
  Text ->
  Either (TryFromException Text WalletType) WalletType
newWalletType = \case
  "exchange" -> Right Exchange
  "margin" -> Right Margin
  "funding" -> Right Funding
  x -> Left $ TryFromException x Nothing

data Response (crel :: CurrencyRelation) = Response
  { balance :: Money crel 'Sell,
    unsettledInterest :: Money crel 'Sell,
    availableBalance :: Money crel 'Sell,
    lastChange :: Maybe Text
  }
  deriving stock
    ( Eq,
      Ord,
      Generic
    )

deriving stock instance
  ( Show (Money crel 'Sell)
  ) =>
  Show (Response crel)
