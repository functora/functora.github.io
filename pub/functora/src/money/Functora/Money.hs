module Functora.Money
  ( Currency (..),
    qqCurrency,
    parseCurrency,
    Money (..),
    qqMoney,
    parseMoney,
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
  { currencyCode :: Text,
    currencyInfo :: Text
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
    [cur] -> pure $ Currency {currencyCode = cur, currencyInfo = mempty}
    _ -> throwParseException input ("Input has wrong amount of words" :: Text)
