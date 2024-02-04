module Functora.Money
  ( CurrencyCode (..),
    qqCurrencyCode,
    parseCurrencyCode,
    Money (..),
    qqMoney,
    parseMoney,
  )
where

import qualified Data.Money as D
import Functora.MoneyOrphan ()
import Functora.Prelude
import qualified Language.Haskell.TH.Syntax as TH

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

data Money = Money
  { moneyAmount :: D.Money Rational,
    moneyCurrencyCode :: CurrencyCode,
    moneyCurrencyDesc :: Text
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
        <*> parseCurrencyCode cur
        <*> pure ""
    _ ->
      throwParseException input ("Input has wrong amount of words" :: Text)
