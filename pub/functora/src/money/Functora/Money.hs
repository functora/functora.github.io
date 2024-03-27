{-# LANGUAGE UndecidableInstances #-}

module Functora.Money
  ( module X,
    MoneyRep,
    MoneyTags,
    NewMoneyTags,
    Money,
    unMoney,
    parseMoney,
    SomeMoney (..),
    newMoney,
    newUnsignedMoneyBOS,
    newUnsignedMoneyGOL,
    newFeeRate,
    addFee,
    deductFee,
    exchangeMoney,
    Funds (..),
    fundsMoneyAmount,
    fundsCurrencyCode,
    unJsonRational,
    unJsonMoney,
    unJsonUnsignedMoneyBOS,
    unJsonUnsignedMoneyGOL,
    CurrencyCode (..),
    inspectCurrencyCode,
    CurrencyInfo (..),
    inspectCurrencyInfo,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson.Combinators.Decode as A
import qualified Data.Text as T
import Functora.MoneyFgpt as X ()
import Functora.MoneySing as X
import Functora.Prelude
import Functora.Tags as X
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
    Integral (MoneyRep sig),
    From (MoneyRep sig) Integer,
    Typeable sig,
    Typeable tags
  )

type NewMoneyTags (sig :: SignedOrUnsigned) lhs rhs =
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

deriving stock instance (MoneyTags sig tags) => Data (Money tags)

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

newMoney ::
  forall tags sig.
  ( MoneyTags sig tags
  ) =>
  Ratio (MoneyRep sig) ->
  Money tags
newMoney = Money

newUnsignedMoneyBOS ::
  forall tags buy sell.
  ( NewMoneyTags 'Unsigned buy (tags |+| 'Unsigned |+| 'Buy),
    NewMoneyTags 'Unsigned sell (tags |+| 'Unsigned |+| 'Sell)
  ) =>
  Rational ->
  SomeMoney BuyOrSell (tags |+| 'Unsigned)
newUnsignedMoneyBOS raw
  | raw < 0 = SomeMoney (sing :: Sing 'Sell) (Money uns :: Money sell)
  | otherwise = SomeMoney (sing :: Sing 'Buy) (Money uns :: Money buy)
  where
    uns = unsafeFrom @Rational @(Ratio Natural) $ abs raw

newUnsignedMoneyGOL ::
  forall tags gain lose.
  ( NewMoneyTags 'Unsigned gain (tags |+| 'Unsigned |+| 'Gain),
    NewMoneyTags 'Unsigned lose (tags |+| 'Unsigned |+| 'Lose)
  ) =>
  Rational ->
  SomeMoney GainOrLose (tags |+| 'Unsigned)
newUnsignedMoneyGOL raw
  | raw < 0 = SomeMoney (sing :: Sing 'Lose) (Money uns :: Money lose)
  | otherwise = SomeMoney (sing :: Sing 'Gain) (Money uns :: Money gain)
  where
    uns = unsafeFrom @Rational @(Ratio Natural) $ abs raw

newFeeRate ::
  forall prev sig next.
  ( NewMoneyTags sig next (prev |+| 'FeeRate)
  ) =>
  Ratio (MoneyRep sig) ->
  Money next
newFeeRate = Money

addFee ::
  forall fee amt sig tags.
  ( GetTag sig fee,
    GetTag sig amt,
    NewMoneyTags
      sig
      tags
      ((fee |-| sig |-| 'FeeRate) |&| (amt |-| 'Net |+| 'Gross))
  ) =>
  Money fee ->
  Money amt ->
  Money tags
addFee (Money fee) (Money amt) =
  Money $ amt / (1 - fee)

deductFee ::
  ( GetTag sig fee,
    GetTag sig amt,
    NewMoneyTags
      sig
      tags
      ((fee |-| sig |-| 'FeeRate) |&| (amt |-| 'Gross |+| 'Net))
  ) =>
  Money fee ->
  Money amt ->
  Money tags
deductFee (Money fee) (Money amt) =
  Money $ amt * (1 - fee)

exchangeMoney ::
  forall rate base quote sig.
  ( MoneyTags sig rate,
    MoneyTags sig base,
    NewMoneyTags
      sig
      quote
      ((rate |-| sig |-| 'QuotePerBase) |&| (base |-| 'Base |+| 'Quote))
  ) =>
  Money rate ->
  Money base ->
  Money quote
exchangeMoney (Money rate) (Money base) =
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

deriving stock instance (MoneyTags sig tags) => Data (Funds tags)

unJsonRational :: A.Decoder Rational
unJsonRational =
  toRational <$> A.scientific

unJsonMoney :: forall tags sig. (MoneyTags sig tags) => A.Decoder (Money tags)
unJsonMoney = do
  rat <- unJsonRational
  case sing :: Sing sig of
    SSigned -> pure $ newMoney rat
    SUnsigned ->
      either (fail . inspect) (pure . newMoney)
        $ tryFrom @Rational @(Ratio Natural) rat

unJsonUnsignedMoneyBOS ::
  forall tags.
  ( MoneyTags 'Unsigned (tags |+| 'Unsigned |+| 'Buy),
    MoneyTags 'Unsigned (tags |+| 'Unsigned |+| 'Sell)
  ) =>
  A.Decoder (SomeMoney BuyOrSell (tags |+| 'Unsigned))
unJsonUnsignedMoneyBOS =
  newUnsignedMoneyBOS @tags <$> unJsonRational

unJsonUnsignedMoneyGOL ::
  forall tags.
  ( MoneyTags 'Unsigned (tags |+| 'Unsigned |+| 'Gain),
    MoneyTags 'Unsigned (tags |+| 'Unsigned |+| 'Lose)
  ) =>
  A.Decoder (SomeMoney GainOrLose (tags |+| 'Unsigned))
unJsonUnsignedMoneyGOL =
  newUnsignedMoneyGOL @tags <$> unJsonRational

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
