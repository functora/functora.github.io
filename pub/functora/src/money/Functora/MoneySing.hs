{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-type-patterns #-}

module Functora.MoneySing where

import Data.Type.Equality
import Functora.Tags
import qualified Language.Haskell.TH.Syntax as TH
import Prelude

data MoneyKind = MoneyAmount | Currency | QuotePerBase | FeeRate | ProfitRate
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data CurrencyKind = Crypto | Stable | Fiat
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data BuyOrSell = Buy | Sell
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data NetOrGross = Net | Gross
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data GainOrLose = Gain | Lose
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data BaseOrQuote = Base | Quote
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data MakerOrTaker = Maker | Taker
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data LocalOrRemote = Local | Remote
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data MinOrMax = Min | Max
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data Revenue = Revenue
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

data SignedOrUnsigned = Signed | Unsigned
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic, TH.Lift)

type instance Fgpt MoneyKind = "Functora.MoneySing.MoneyKind"

type instance Fgpt CurrencyKind = "Functora.MoneySing.CurrencyKind"

type instance Fgpt BuyOrSell = "Functora.MoneySing.BuyOrSell"

type instance Fgpt NetOrGross = "Functora.MoneySing.NetOrGross"

type instance Fgpt GainOrLose = "Functora.MoneySing.GainOrLose"

type instance Fgpt BaseOrQuote = "Functora.MoneySing.BaseOrQuote"

type instance Fgpt MakerOrTaker = "Functora.MoneySing.MakerOrTaker"

type instance Fgpt LocalOrRemote = "Functora.MoneySing.LocalOrRemote"

type instance Fgpt MinOrMax = "Functora.MoneySing.MinOrMax"

type instance Fgpt Revenue = "Functora.MoneySing.Revenue"

type instance Fgpt SignedOrUnsigned = "Functora.MoneySing.SignedOrUnsigned"

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
