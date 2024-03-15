{-# LANGUAGE UndecidableInstances #-}

module Functora.Money
  ( module X,
    Money (..),
    unMoney,
    parseMoney,
    SomeMoney (..),
    mkSignedMoney,
    mkUnsignedMoney,
    mkFeeRate,
    deductFee,
    quoteFromBase,
    Funds (..),
    fundsMoneyAmount,
    fundsCurrencyCode,
    unJsonRational,
    unJsonSignedMoney,
    unJsonUnsignedMoney,
    CurrencyCode (..),
    inspectCurrencyCode,
    CurrencyInfo (..),
    inspectCurrencyInfo,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson.Combinators.Decode as A
import qualified Data.Text as T
import Functora.MoneyOrphan as X ()
import Functora.MoneySing as X
import Functora.Prelude
import Functora.Tags
import qualified Language.Haskell.TH.Syntax as TH

type family MoneyRep sig where
  MoneyRep 'Signed = Integer
  MoneyRep 'Unsigned = Natural

type MoneyTags (sig :: SignedOrUnsigned) tags =
  ( GetTag sig tags,
    Eq (MoneyRep sig),
    Ord (MoneyRep sig),
    Show (MoneyRep sig),
    Read (MoneyRep sig),
    Data (MoneyRep sig),
    Integral (MoneyRep sig)
  )

type MkMoneyTags (sig :: SignedOrUnsigned) lhs rhs =
  ( lhs ~ rhs,
    MoneyTags sig lhs
  )

data Money tags where
  Money ::
    forall tags sig.
    ( MoneyTags sig tags
    ) =>
    Ratio (MoneyRep sig) ->
    Money tags

deriving stock instance Eq (Money tags)

deriving stock instance Ord (Money tags)

deriving stock instance (MoneyTags sig tags) => Show (Money tags)

deriving stock instance (MoneyTags sig tags) => Read (Money tags)

deriving stock instance
  ( MoneyTags sig tags,
    Typeable tags
  ) =>
  Data (Money tags)

unMoney ::
  forall tags sig.
  ( MoneyTags sig tags
  ) =>
  Money tags ->
  Ratio (MoneyRep sig)
unMoney (Money x) = x

parseMoney ::
  forall str tags m sig rep.
  ( From str Text,
    From rep Integer,
    Show str,
    Data str,
    MonadThrow m,
    MoneyTags sig tags,
    rep ~ MoneyRep sig
  ) =>
  str ->
  m (Money tags)
parseMoney =
  fmap Money . parseRatio

data SomeMoney k tags
  = forall (tag :: k) (sig :: SignedOrUnsigned).
    ( SingI tag,
      Typeable tag,
      Typeable k,
      MoneyTags sig (tags |+| tag)
    ) =>
    SomeMoney
      (Sing tag)
      (Money (tags |+| tag))

instance (TestEquality (Sing :: k -> Type)) => Eq (SomeMoney k tags) where
  (SomeMoney sx x) == (SomeMoney sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeMoney k tags)

mkSignedMoney ::
  forall prev next.
  ( MkMoneyTags 'Signed next (prev |+| 'Signed)
  ) =>
  Rational ->
  Money next
mkSignedMoney = Money

mkUnsignedMoney ::
  forall tags gain lose.
  ( MkMoneyTags 'Unsigned gain (tags |+| 'Unsigned |+| 'Gain),
    MkMoneyTags 'Unsigned lose (tags |+| 'Unsigned |+| 'Lose)
  ) =>
  Rational ->
  SomeMoney GainOrLose (tags |+| 'Unsigned)
mkUnsignedMoney raw
  | raw < 0 =
      SomeMoney (sing :: Sing 'Lose) (Money uns :: Money lose)
  | otherwise =
      SomeMoney (sing :: Sing 'Gain) (Money uns :: Money gain)
  where
    uns = unsafeFrom @Rational @(Ratio Natural) $ abs raw

mkFeeRate ::
  forall prev sig next.
  ( MkMoneyTags sig next (prev |+| 'Unsigned |+| 'FeeRate)
  ) =>
  Ratio (MoneyRep sig) ->
  Money next
mkFeeRate = Money

deductFee ::
  forall fee amt sig tags.
  ( GetTag sig fee,
    GetTag sig amt,
    MkMoneyTags
      sig
      tags
      ((fee |-| sig |-| 'FeeRate) |&| (amt |-| 'Gross |+| 'Net))
  ) =>
  Money fee ->
  Money amt ->
  Money tags
deductFee (Money fee) (Money amt) =
  Money $ amt * (1 - fee)

quoteFromBase ::
  forall rate base quote sig.
  ( MoneyTags sig rate,
    MoneyTags sig base,
    MkMoneyTags
      sig
      quote
      ((rate |-| sig |-| 'QuotePerBase) |&| (base |-| 'Base |+| 'Quote))
  ) =>
  Money rate ->
  Money base ->
  Money quote
quoteFromBase (Money rate) (Money base) =
  Money $ rate * base

data Funds tags where
  Funds ::
    forall tags sig.
    ( MoneyTags sig tags
    ) =>
    Money tags ->
    CurrencyCode ->
    Funds tags

fundsMoneyAmount :: Funds tags -> Money tags
fundsMoneyAmount (Funds amt _) = amt

fundsCurrencyCode :: Funds tags -> CurrencyCode
fundsCurrencyCode (Funds _ cur) = cur

deriving stock instance Eq (Funds tags)

deriving stock instance Ord (Funds tags)

deriving stock instance (MoneyTags sig tags) => Show (Funds tags)

deriving stock instance (MoneyTags sig tags) => Read (Funds tags)

deriving stock instance (MoneyTags sig tags, Typeable tags) => Data (Funds tags)

unJsonRational :: A.Decoder Rational
unJsonRational = toRational <$> A.scientific

unJsonSignedMoney :: (MoneyTags 'Signed tags) => A.Decoder (Money tags)
unJsonSignedMoney = Money <$> unJsonRational

unJsonUnsignedMoney ::
  forall tags.
  ( MoneyTags 'Unsigned (tags |+| 'Unsigned |+| 'Gain),
    MoneyTags 'Unsigned (tags |+| 'Unsigned |+| 'Lose)
  ) =>
  A.Decoder (SomeMoney GainOrLose (tags |+| 'Unsigned))
unJsonUnsignedMoney =
  mkUnsignedMoney @tags <$> unJsonRational

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
