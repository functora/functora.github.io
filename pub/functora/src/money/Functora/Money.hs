{-# LANGUAGE UndecidableInstances #-}

module Functora.Money
  ( module X,
    IntRep,
    MoneyTags,
    NewMoneyTags,
    Money,
    unMoney,
    tagMoney,
    unTagMoney,
    reTagMoney,
    parseMoney,
    addMoney,
    deductMoney,
    SomeMoney (..),
    newMoney,
    newUnsignedMoneyBOS,
    newUnsignedMoneyGOL,
    newFeeRate,
    newProfitRate,
    addFee,
    deductFee,
    addProfit,
    exchangeMoney,
    newQuotePerBase,
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

type IntRep tags = NewIntRep (GetKey SignedOrUnsigned tags)

type family NewIntRep sig where
  NewIntRep 'Signed = Integer
  NewIntRep 'Unsigned = Natural

type MoneyTags tags =
  ( Eq (IntRep tags),
    Ord (IntRep tags),
    Show (IntRep tags),
    Read (IntRep tags),
    Data (IntRep tags),
    Integral (IntRep tags),
    From (IntRep tags) Integer,
    Typeable (IntRep tags),
    Typeable tags
  )

type NewMoneyTags lhs rhs =
  ( lhs ~ rhs,
    MoneyTags lhs
  )

data Money tags where
  Money ::
    forall tags.
    ( MoneyTags tags
    ) =>
    Ratio (IntRep tags) ->
    Money tags

deriving stock instance Eq (Money tags)

deriving stock instance Ord (Money tags)

deriving stock instance (MoneyTags tags) => Show (Money tags)

deriving stock instance (MoneyTags tags) => Read (Money tags)

deriving stock instance (MoneyTags tags) => Data (Money tags)

unMoney :: Money tags -> Ratio (IntRep tags)
unMoney (Money x) = x

tagMoney ::
  forall prevTag prevTags nextTags.
  ( NewMoneyTags nextTags (prevTags |+| prevTag),
    IntRep prevTags ~ IntRep nextTags
  ) =>
  Money prevTags ->
  Money nextTags
tagMoney = newMoney . unMoney

unTagMoney ::
  forall prevTag prevTags nextTags.
  ( NewMoneyTags nextTags (prevTags |-| prevTag),
    IntRep prevTags ~ IntRep nextTags
  ) =>
  Money prevTags ->
  Money nextTags
unTagMoney = newMoney . unMoney

reTagMoney ::
  forall prevTag nextTag prevTags nextTags.
  ( NewMoneyTags nextTags (prevTags |-| prevTag |+| nextTag),
    IntRep prevTags ~ IntRep nextTags
  ) =>
  Money prevTags ->
  Money nextTags
reTagMoney = newMoney . unMoney

parseMoney ::
  forall str tags m.
  ( From str Text,
    Show str,
    Data str,
    MoneyTags tags,
    MonadThrow m
  ) =>
  str ->
  m (Money tags)
parseMoney =
  fmap Money . parseRatio

addMoney :: (MoneyTags tags) => Money tags -> Money tags -> Money tags
addMoney lhs rhs =
  newMoney $ unMoney lhs + unMoney rhs

deductMoney :: (MoneyTags tags) => Money tags -> Money tags -> Money tags
deductMoney lhs rhs =
  newMoney $ unMoney lhs - unMoney rhs

data SomeMoney k tags
  = forall (tag :: k).
    ( SingI tag,
      Typeable tag,
      Typeable k,
      MoneyTags (tags |+| tag)
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
  forall tags.
  ( MoneyTags tags
  ) =>
  Ratio (IntRep tags) ->
  Money tags
newMoney = Money

newUnsignedMoneyBOS ::
  forall tags buy sell.
  ( NewMoneyTags buy (tags |+| 'Unsigned |+| 'Buy),
    NewMoneyTags sell (tags |+| 'Unsigned |+| 'Sell),
    IntRep buy ~ Natural,
    IntRep sell ~ Natural
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
  ( NewMoneyTags gain (tags |+| 'Unsigned |+| 'Gain),
    NewMoneyTags lose (tags |+| 'Unsigned |+| 'Lose),
    IntRep gain ~ Natural,
    IntRep lose ~ Natural
  ) =>
  Rational ->
  SomeMoney GainOrLose (tags |+| 'Unsigned)
newUnsignedMoneyGOL raw
  | raw < 0 = SomeMoney (sing :: Sing 'Lose) (Money uns :: Money lose)
  | otherwise = SomeMoney (sing :: Sing 'Gain) (Money uns :: Money gain)
  where
    uns = unsafeFrom @Rational @(Ratio Natural) $ abs raw

newFeeRate ::
  forall prev next.
  ( NewMoneyTags next (prev |+| 'FeeRate)
  ) =>
  Ratio (IntRep next) ->
  Money next
newFeeRate = Money

newProfitRate ::
  forall prev next.
  ( NewMoneyTags next (prev |+| 'ProfitRate)
  ) =>
  Ratio (IntRep next) ->
  Money next
newProfitRate = Money

addFee ::
  forall fee amt tags.
  ( IntRep fee ~ IntRep amt,
    IntRep fee ~ IntRep tags,
    NewMoneyTags tags (amt |-| 'Net |+| 'Gross)
  ) =>
  Money fee ->
  Money amt ->
  Money tags
addFee (Money fee) (Money amt) =
  Money $ amt / (1 - fee)

deductFee ::
  forall fee amt tags.
  ( IntRep fee ~ IntRep amt,
    IntRep fee ~ IntRep tags,
    NewMoneyTags tags (amt |-| 'Gross |+| 'Net)
  ) =>
  Money fee ->
  Money amt ->
  Money tags
deductFee (Money fee) (Money amt) =
  Money $ amt * (1 - fee)

addProfit ::
  forall tags.
  ( IntRep tags ~ IntRep (tags |+| 'ProfitRate),
    IntRep tags ~ IntRep (tags |+| 'Revenue),
    MoneyTags (tags |+| 'Revenue)
  ) =>
  Money (tags |+| 'ProfitRate) ->
  Money tags ->
  Money (tags |+| 'Revenue)
addProfit (Money rate) (Money amt) =
  Money $ amt * (1 + rate)

exchangeMoney ::
  forall tags.
  ( IntRep (tags |+| 'QuotePerBase) ~ IntRep (tags |+| 'Base),
    IntRep (tags |+| 'QuotePerBase) ~ IntRep (tags |+| 'Quote),
    MoneyTags (tags |+| 'Quote)
  ) =>
  Money (tags |+| 'QuotePerBase) ->
  Money (tags |+| 'Base) ->
  Money (tags |+| 'Quote)
exchangeMoney (Money rate) (Money base) =
  Money $ rate * base

newQuotePerBase ::
  forall tags.
  ( IntRep (tags |+| 'Quote) ~ IntRep (tags |+| 'Base),
    IntRep (tags |+| 'Quote) ~ IntRep (tags |+| 'QuotePerBase),
    MoneyTags (tags |+| 'QuotePerBase)
  ) =>
  Money (tags |+| 'Quote) ->
  Money (tags |+| 'Base) ->
  Money (tags |+| 'QuotePerBase)
newQuotePerBase (Money quote) (Money base) =
  Money $ quote / base

data Funds tags where
  Funds :: Money tags -> CurrencyCode -> Funds tags

fundsMoneyAmount :: Funds tags -> Money tags
fundsMoneyAmount (Funds amt _) = amt

fundsCurrencyCode :: Funds tags -> CurrencyCode
fundsCurrencyCode (Funds _ cur) = cur

deriving stock instance Eq (Funds tags)

deriving stock instance Ord (Funds tags)

deriving stock instance (MoneyTags tags) => Show (Funds tags)

deriving stock instance (MoneyTags tags) => Read (Funds tags)

deriving stock instance (MoneyTags tags) => Data (Funds tags)

unJsonRational :: A.Decoder Rational
unJsonRational = toRational <$> A.scientific

unJsonMoney ::
  forall tags (sig :: SignedOrUnsigned).
  ( MoneyTags tags,
    GetTag sig tags
  ) =>
  A.Decoder (Money tags)
unJsonMoney = do
  rat <- unJsonRational
  case sing :: Sing sig of
    SSigned -> pure $ newMoney rat
    SUnsigned ->
      either (fail . inspect) (pure . newMoney)
        $ tryFrom @Rational @(Ratio Natural) rat

unJsonUnsignedMoneyBOS ::
  forall tags buy sell.
  ( NewMoneyTags buy (tags |+| 'Unsigned |+| 'Buy),
    NewMoneyTags sell (tags |+| 'Unsigned |+| 'Sell),
    IntRep buy ~ Natural,
    IntRep sell ~ Natural
  ) =>
  A.Decoder (SomeMoney BuyOrSell (tags |+| 'Unsigned))
unJsonUnsignedMoneyBOS =
  newUnsignedMoneyBOS @tags <$> unJsonRational

unJsonUnsignedMoneyGOL ::
  forall tags gain lose.
  ( NewMoneyTags gain (tags |+| 'Unsigned |+| 'Gain),
    NewMoneyTags lose (tags |+| 'Unsigned |+| 'Lose),
    IntRep gain ~ Natural,
    IntRep lose ~ Natural
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
