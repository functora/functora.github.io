{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.MoneySing where

import Data.Type.Equality
import Functora.Cfg
import Functora.Prelude
import qualified Language.Haskell.TH.Syntax as TH
import Singlethongs

data CurrencyKind = Crypto | Stable | Fiat
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum CurrencyKind

data BuyOrSell = Buy | Sell
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum BuyOrSell

data NetOrGross = Net | Gross
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum NetOrGross

data GainOrLose = Gain | Lose
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum GainOrLose

data BaseOrQuote = Base | Quote
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum BaseOrQuote

data MakerOrTaker = Maker | Taker
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum MakerOrTaker

data LocalOrRemote = Local | Remote
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum LocalOrRemote

data MinOrMax = Min | Max
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum MinOrMax

data Revenue = Revenue
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum Revenue

data SignedOrUnsigned = Signed | Unsigned
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)
  deriving (HasCodec, HasItemCodec) via GenericEnum SignedOrUnsigned

--
-- SignedOrUnsigned
--

data instance Sing (x :: SignedOrUnsigned) where
  SSigned :: Sing 'Signed
  SUnsigned :: Sing 'Unsigned

instance SingKind SignedOrUnsigned where
  type Demote SignedOrUnsigned = SignedOrUnsigned
  fromSing SSigned = Signed
  fromSing SUnsigned = Unsigned
  toSing Signed = SomeSing SSigned
  toSing Unsigned = SomeSing SUnsigned

instance SingI 'Signed where sing = SSigned

instance SingI 'Unsigned where sing = SUnsigned

instance TestEquality (Sing :: SignedOrUnsigned -> Type) where
  testEquality SSigned SSigned = Just Refl
  testEquality SUnsigned SUnsigned = Just Refl
  testEquality _ _ = Nothing

--
-- BuyOrSell
--

data instance Sing (x :: BuyOrSell) where
  SBuy :: Sing 'Buy
  SSell :: Sing 'Sell

instance SingKind BuyOrSell where
  type Demote BuyOrSell = BuyOrSell
  fromSing SBuy = Buy
  fromSing SSell = Sell
  toSing Buy = SomeSing SBuy
  toSing Sell = SomeSing SSell

instance SingI 'Buy where sing = SBuy

instance SingI 'Sell where sing = SSell

instance TestEquality (Sing :: BuyOrSell -> Type) where
  testEquality SBuy SBuy = Just Refl
  testEquality SSell SSell = Just Refl
  testEquality _ _ = Nothing

--
-- GainOrLose
--

data instance Sing (x :: GainOrLose) where
  SGain :: Sing 'Gain
  SLose :: Sing 'Lose

instance SingKind GainOrLose where
  type Demote GainOrLose = GainOrLose
  fromSing SGain = Gain
  fromSing SLose = Lose
  toSing Gain = SomeSing SGain
  toSing Lose = SomeSing SLose

instance SingI 'Gain where sing = SGain

instance SingI 'Lose where sing = SLose

instance TestEquality (Sing :: GainOrLose -> Type) where
  testEquality SGain SGain = Just Refl
  testEquality SLose SLose = Just Refl
  testEquality _ _ = Nothing

--
-- BaseOrQuote
--

data instance Sing (x :: BaseOrQuote) where
  SBase :: Sing 'Base
  SQuote :: Sing 'Quote

instance SingKind BaseOrQuote where
  type Demote BaseOrQuote = BaseOrQuote
  fromSing SBase = Base
  fromSing SQuote = Quote
  toSing Base = SomeSing SBase
  toSing Quote = SomeSing SQuote

instance SingI 'Base where sing = SBase

instance SingI 'Quote where sing = SQuote

instance TestEquality (Sing :: BaseOrQuote -> Type) where
  testEquality SBase SBase = Just Refl
  testEquality SQuote SQuote = Just Refl
  testEquality _ _ = Nothing

--
-- MakerOrTaker
--

data instance Sing (x :: MakerOrTaker) where
  SMaker :: Sing 'Maker
  STaker :: Sing 'Taker

instance SingKind MakerOrTaker where
  type Demote MakerOrTaker = MakerOrTaker
  fromSing SMaker = Maker
  fromSing STaker = Taker
  toSing Maker = SomeSing SMaker
  toSing Taker = SomeSing STaker

instance SingI 'Maker where sing = SMaker

instance SingI 'Taker where sing = STaker

instance TestEquality (Sing :: MakerOrTaker -> Type) where
  testEquality SMaker SMaker = Just Refl
  testEquality STaker STaker = Just Refl
  testEquality _ _ = Nothing
