{-# LANGUAGE UndecidableInstances #-}

module Functora.Money
  ( module X,
    Buck (..),
    unBuck,
    mkUnsignedCash,
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
import Functora.MoneySing hiding (Money (..))
import Functora.Prelude
import Functora.Tags
import qualified Language.Haskell.TH.Syntax as TH

type family BuckRep tag where
  BuckRep 'Signed = Integer
  BuckRep 'Unsigned = Natural

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
  = forall (tag :: k) (sos :: SignedOrUnsigned).
    ( SingI tag,
      Typeable tag,
      Typeable k,
      GetTag sos (tags |+| tag),
      Eq (BuckRep sos),
      Ord (BuckRep sos),
      Show (BuckRep sos),
      Read (BuckRep sos),
      Data (BuckRep sos),
      Integral (BuckRep sos)
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

data Cash tags where
  SignedCash :: (HasTag 'Signed tags) => Rational -> Cash tags
  UnsignedCash :: (HasTag 'Unsigned tags) => Ratio Natural -> Cash tags

deriving stock instance Eq (Cash tags)

deriving stock instance Ord (Cash tags)

deriving stock instance Show (Cash tags)

data SomeCash k tags
  = forall (tag :: k).
    ( SingI tag,
      Typeable tag,
      Typeable k
    ) =>
    SomeCash
      (Sing tag)
      (Cash (tags |+| tag))

instance (TestEquality (Sing :: k -> Type)) => Eq (SomeCash k tags) where
  (SomeCash sx x) == (SomeCash sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeCash k tags)

mkUnsignedCash ::
  forall tags next gain lose.
  ( next ~ (tags |+| 'Unsigned),
    gain ~ (next |+| 'Gain),
    lose ~ (next |+| 'Lose),
    HasNotKey GainOrLose next,
    HasTag 'Unsigned gain,
    HasTag 'Unsigned lose
  ) =>
  Rational ->
  SomeCash GainOrLose (tags |+| 'Unsigned)
mkUnsignedCash raw
  | raw < 0 = SomeCash (sing :: Sing 'Lose) (UnsignedCash uns :: Cash lose)
  | raw > 0 = SomeCash (sing :: Sing 'Gain) (UnsignedCash uns :: Cash gain)
  | otherwise = SomeCash (sing :: Sing 'Gain) (UnsignedCash uns :: Cash gain)
  where
    uns = unsafeFrom @Rational @(Ratio Natural) $ abs raw

mkFeeRate ::
  forall tags.
  Rational ->
  Tagged (tags |+| 'FeeRate) Rational
mkFeeRate = Tagged

deductFee ::
  forall fee amt.
  Tagged fee Rational ->
  Tagged amt Rational ->
  Tagged ((fee |-| 'FeeRate) |&| (amt |-| 'Gross |+| 'Net)) Rational
deductFee (Tagged fee) (Tagged amt) =
  Tagged $ amt * (1 - fee)

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
