{-# LANGUAGE UndecidableInstances #-}

module Functora.Money
  ( module X,
    Money (..),
    MoneyAmount (..),
    QuotePerBase (..),
    CurrencyCode (..),
    inspectCurrencyCode,
    CurrencyInfo (..),
    inspectCurrencyInfo,
    FeeRate (..),
    ProfitRate (..),
    unJsonRational,
    unJsonRatio,
  )
where

import qualified Data.Aeson.Combinators.Decode as A
import Functora.Cfg
import Functora.MoneySing as X
import Functora.Prelude
import Functora.Tags as X

data Money = Money
  { moneyAmount :: MoneyAmount,
    moneyCurrencyCode :: CurrencyCode
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving
    ( Binary,
      ToJSON,
      FromJSON,
      HasCodec,
      HasItemCodec
    )
    via GenericType Money

newtype MoneyAmount = MoneyAmount
  { unMoneyAmount :: Ratio Natural
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype
    ( Binary,
      ToJSON,
      FromJSON,
      HasCodec,
      HasItemCodec
    )

newtype QuotePerBase = QuotePerBase
  { unQuotePerBase :: Ratio Natural
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype
    ( Binary,
      ToJSON,
      FromJSON,
      HasCodec,
      HasItemCodec
    )

newtype CurrencyCode = CurrencyCode
  { unCurrencyCode :: Unicode
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype
    ( Binary,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey,
      HasCodec,
      HasItemCodec
    )

inspectCurrencyCode :: CurrencyCode -> Unicode
inspectCurrencyCode = strip . unCurrencyCode

data CurrencyInfo = CurrencyInfo
  { currencyInfoCode :: CurrencyCode,
    currencyInfoText :: Unicode
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving
    ( Binary,
      ToJSON,
      FromJSON,
      HasCodec,
      HasItemCodec
    )
    via GenericType CurrencyInfo

inspectCurrencyInfo :: CurrencyInfo -> Unicode
inspectCurrencyInfo input =
  if null info
    then code
    else code <> " - " <> info
  where
    info = strip $ currencyInfoText input
    code = inspectCurrencyCode $ currencyInfoCode input

newtype FeeRate = FeeRate
  { unFeeRate :: Ratio Natural
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype
    ( Binary,
      ToJSON,
      FromJSON,
      HasCodec,
      HasItemCodec
    )

newtype ProfitRate = ProfitRate
  { unProfitRate :: Ratio Natural
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving newtype
    ( Binary,
      ToJSON,
      FromJSON,
      HasCodec,
      HasItemCodec
    )

unJsonRational :: A.Decoder Rational
unJsonRational = toRational <$> A.scientific

unJsonRatio ::
  forall a.
  ( Data a,
    Integral a,
    TryFrom Integer a
  ) =>
  A.Decoder (Ratio a)
unJsonRatio = do
  rat <- unJsonRational
  either (fail . inspect) pure
    $ tryFrom @Rational @(Ratio a) rat
