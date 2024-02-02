{-# LANGUAGE TemplateHaskell #-}

module Functora.Money
  ( CurrencyCode,
    unCurrencyCode,
    qqCurrencyCode,
    parseCurrencyCode,
    Money,
    moneyAmount,
    moneyCurrencyCode,
    moneyCurrencyDesc,
    qqMoney,
    parseMoney,
  )
where

import qualified Data.Money as D
import Functora.MoneyOrphan ()
import Functora.Prelude

newtype CurrencyCode = CurrencyCode
  { _unCurrencyCode :: Text
  }
  deriving newtype (Eq, Ord, Show, Read)
  deriving stock (Data, Generic, LiftTH)

mkGetters ''CurrencyCode

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

data Money = Money
  { _moneyAmount :: D.Money Rational,
    _moneyCurrencyCode :: CurrencyCode,
    _moneyCurrencyDesc :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, LiftTH)

mkGetters ''Money

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
        <*> parseCurrencyCode cur
        <*> pure ""
    _ ->
      throwParseException input ("Input has wrong amount of words" :: Text)
