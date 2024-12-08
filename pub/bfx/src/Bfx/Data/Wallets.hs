{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Wallets
  ( WalletType (..),
    newWalletType,
    Response (..),
  )
where

import Bfx.Import.External

data WalletType
  = Exchange
  | Margin
  | Funding
  deriving stock
    ( Eq,
      Ord,
      Show,
      Data,
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

data Response = Response
  { balance :: MoneyAmount,
    unsettledInterest :: MoneyAmount,
    availableBalance :: MoneyAmount,
    lastChange :: Maybe Text
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )
