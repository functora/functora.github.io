module Functora.Money
  ( module X,
    Funds (..),
    unJsonRational,
    unJsonMoney,
    CurrencyCode (..),
    CurrencyInfo (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson.Combinators.Decode as A
import Data.Money as X
import Functora.MoneyOrphan ()
import Functora.Prelude
import qualified Language.Haskell.TH.Syntax as TH

data Funds = Funds
  { fundsMoneyAmount :: Money Rational,
    fundsCurrencyCode :: CurrencyCode
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)

unJsonRational :: A.Decoder Rational
unJsonRational = toRational <$> A.scientific

unJsonMoney :: A.Decoder (Money Rational)
unJsonMoney = review money <$> unJsonRational

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
