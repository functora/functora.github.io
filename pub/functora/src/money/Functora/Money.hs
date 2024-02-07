module Functora.Money
  ( Money (..),
    qqMoney,
    parseMoney,
    Currency (..),
    qqCurrency,
    parseCurrency,
    CurrencyCode (..),
    qqCurrencyCode,
    parseCurrencyCode,
    CurrencyInfo (..),
  )
where

import qualified Data.Money as D
import Functora.MoneyOrphan ()
import Functora.Prelude
import qualified Language.Haskell.TH.Syntax as TH

data Money = Money
  { moneyAmount :: D.Money Rational,
    moneyCurrency :: Currency
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)

qqMoney :: QuasiQuoter
qqMoney = qq @Text @Money parseMoney

parseMoney ::
  ( From a Text,
    Show a,
    Data a,
    MonadThrow m
  ) =>
  a ->
  m Money
parseMoney input =
  case parseWords input of
    [amt, cur] ->
      Money
        <$> fmap (review D.money) (parseRatio amt)
        <*> parseCurrency cur
    _ ->
      throwParseException input ("Input has wrong amount of words" :: Text)

data Currency = Currency
  { currencyCode :: CurrencyCode,
    currencyInfo :: CurrencyInfo
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)

qqCurrency :: QuasiQuoter
qqCurrency = qq @Text @Currency parseCurrency

parseCurrency ::
  ( From a Text,
    Show a,
    Data a,
    MonadThrow m
  ) =>
  a ->
  m Currency
parseCurrency input =
  case parseWords input of
    [cur] -> Currency <$> parseCurrencyCode cur <*> pure (CurrencyInfo mempty)
    _ -> throwParseException input ("Input has wrong amount of words" :: Text)

newtype CurrencyCode = CurrencyCode
  { unCurrencyCode :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)

qqCurrencyCode :: QuasiQuoter
qqCurrencyCode = qq @Text @CurrencyCode parseCurrencyCode

parseCurrencyCode ::
  ( From a Text,
    Show a,
    Data a,
    MonadThrow m
  ) =>
  a ->
  m CurrencyCode
parseCurrencyCode input =
  case parseWords input of
    [cur] -> pure $ CurrencyCode cur
    _ -> throwParseException input ("Input has wrong amount of words" :: Text)

newtype CurrencyInfo = CurrencyInfo
  { unCurrencyInfo :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)
