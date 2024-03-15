{-# LANGUAGE UndecidableInstances #-}

module Functora.Money
  ( module X,
    Buck (..),
    unBuck,
    SomeBuck (..),
    mkUnsignedBuck,
    mkFeeRate,
    deductFee,
    unMoney,
    parseMoney,
    Funds (..),
    unJsonRational,
    unJsonMoney,
    CurrencyCode (..),
    inspectCurrencyCode,
    CurrencyInfo (..),
    inspectCurrencyInfo,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson.Combinators.Decode as A
import Data.Money as X
import qualified Data.Text as T
import Functora.MoneyOrphan ()
import Functora.MoneySing
import Functora.Prelude
import Functora.Tags
import qualified Language.Haskell.TH.Syntax as TH

type family BuckRep tag where
  BuckRep 'Signed = Integer
  BuckRep 'Unsigned = Natural

type BuckTags (sig :: SignedOrUnsigned) lhs rhs =
  ( lhs ~ rhs,
    GetTag sig lhs,
    Integral (BuckRep sig)
  )

data Buck tags where
  Buck ::
    forall tags tag.
    ( GetTag (tag :: SignedOrUnsigned) tags,
      Integral (BuckRep tag)
    ) =>
    Ratio (BuckRep tag) ->
    Buck tags

deriving stock instance
  ( GetTag (tag :: SignedOrUnsigned) tags,
    Integral (BuckRep tag)
  ) =>
  Eq (Buck tags)

deriving stock instance
  ( GetTag (tag :: SignedOrUnsigned) tags,
    Integral (BuckRep tag)
  ) =>
  Ord (Buck tags)

deriving stock instance
  ( GetTag (tag :: SignedOrUnsigned) tags,
    Integral (BuckRep tag),
    Show (BuckRep tag)
  ) =>
  Show (Buck tags)

deriving stock instance
  ( GetTag (tag :: SignedOrUnsigned) tags,
    Integral (BuckRep tag),
    Read (BuckRep tag)
  ) =>
  Read (Buck tags)

deriving stock instance
  ( GetTag (tag :: SignedOrUnsigned) tags,
    Integral (BuckRep tag),
    Data (BuckRep tag),
    Typeable tags
  ) =>
  Data (Buck tags)

unBuck ::
  forall tags tag.
  ( GetTag (tag :: SignedOrUnsigned) tags
  ) =>
  Buck tags ->
  Ratio (BuckRep tag)
unBuck (Buck x) = x

data SomeBuck k tags
  = forall (tag :: k) (sig :: SignedOrUnsigned).
    ( SingI tag,
      Typeable tag,
      Typeable k,
      GetTag sig (tags |+| tag),
      Eq (BuckRep sig),
      Ord (BuckRep sig),
      Show (BuckRep sig),
      Read (BuckRep sig),
      Data (BuckRep sig),
      Integral (BuckRep sig)
    ) =>
    SomeBuck
      (Sing tag)
      (Buck (tags |+| tag))

instance (TestEquality (Sing :: k -> Type)) => Eq (SomeBuck k tags) where
  (SomeBuck sx x) == (SomeBuck sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeBuck k tags)

--
-- TODO : mkSignedBuck
--

mkUnsignedBuck ::
  forall tags gain lose.
  ( BuckTags 'Unsigned gain (tags |+| 'Unsigned |+| 'Gain),
    BuckTags 'Unsigned lose (tags |+| 'Unsigned |+| 'Lose)
  ) =>
  Rational ->
  SomeBuck GainOrLose (tags |+| 'Unsigned)
mkUnsignedBuck raw
  | raw < 0 = SomeBuck (sing :: Sing 'Lose) (Buck uns :: Buck lose)
  | otherwise = SomeBuck (sing :: Sing 'Gain) (Buck uns :: Buck gain)
  where
    uns = unsafeFrom @Rational @(Ratio Natural) $ abs raw

mkFeeRate ::
  forall prev sig next.
  ( BuckTags sig next (prev |+| 'Unsigned |+| 'FeeRate)
  ) =>
  Ratio (BuckRep sig) ->
  Buck next
mkFeeRate = Buck

deductFee ::
  forall fee amt sig tags.
  ( GetTag sig fee,
    GetTag sig amt,
    BuckTags sig tags ((fee |-| 'FeeRate) |&| (amt |-| 'Gross |+| 'Net))
  ) =>
  Buck fee ->
  Buck amt ->
  Buck tags
deductFee (Buck fee) (Buck amt) =
  Buck $ amt * (1 - fee)

unMoney :: Money a -> a
unMoney (Money x) = x

parseMoney ::
  forall str int m.
  ( From str Text,
    From int Integer,
    Integral int,
    Show str,
    Show int,
    Data str,
    Data int,
    MonadThrow m
  ) =>
  str ->
  m (Money (Ratio int))
parseMoney =
  fmap Money . parseRatio

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

inspectCurrencyCode :: forall a. (From Text a) => CurrencyCode -> a
inspectCurrencyCode =
  from @Text @a
    . T.strip
    . unCurrencyCode

data CurrencyInfo = CurrencyInfo
  { currencyInfoCode :: CurrencyCode,
    currencyInfoText :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic, TH.Lift)

inspectCurrencyInfo :: forall a. (From Text a) => CurrencyInfo -> a
inspectCurrencyInfo input =
  from @Text @a
    $ if null info
      then code
      else code <> " - " <> info
  where
    info = T.strip $ currencyInfoText input
    code = inspectCurrencyCode $ currencyInfoCode input
