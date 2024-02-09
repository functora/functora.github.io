module Functora.Money
  ( Money (..),
    unJsonRational,
    unJsonMoneyAmount,
    CurrencyCode (..),
    CurrencyInfo (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson.Combinators.Decode as A
import qualified Data.Money as D
import Functora.MoneyOrphan ()
import Functora.Prelude
import qualified Language.Haskell.TH.Syntax as TH

data Money = Money
  { moneyAmount :: D.Money Rational,
    moneyCurrencyCode :: CurrencyCode
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)

unJsonRational :: A.Decoder Rational
unJsonRational = toRational <$> A.scientific

unJsonMoneyAmount :: A.Decoder (D.Money Rational)
unJsonMoneyAmount = review D.money <$> unJsonRational

newtype CurrencyCode = CurrencyCode
  { unCurrencyCode :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

data CurrencyInfo = CurrencyInfo
  { currencyInfoCode :: CurrencyCode,
    currencyInfoText :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)
