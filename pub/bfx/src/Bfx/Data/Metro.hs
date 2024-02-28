{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Metro
  ( -- | Primary money types
    Money,
    unMoney,
    unMoney',
    SomeMoney (..),
    -- | Metrology compatible money type aliases
    MoneyBase',
    MoneyQuote',
    -- | Primary exchange rate types
    QuotePerBase,
    unQuotePerBase,
    SomeQuotePerBase (..),
    -- | Metrology compatible exchange rate type alias
    QuotePerBase',
    unQuotePerBase',
    mkQuotePerBase',
    unUnitless,
    -- | Lossy primitive constructors
    roundMoney,
    roundQuotePerBase,
    -- | Lossy metrology compatible constructors
    roundMoney',
    roundQuotePerBase',
    -- | QuasiQuoters for literals in code
    moneyBaseBuy,
    moneyBaseSell,
    moneyQuoteBuy,
    moneyQuoteSell,
    quotePerBaseBuy,
    quotePerBaseSell,
  )
where

import Bfx.Class.ToRequestParam
import Bfx.Data.Kind
import Bfx.Import.External hiding ((%))
import Bfx.Util
import qualified Data.Aeson as A
import Data.Metrology.Poly
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import qualified Witch
import qualified Prelude

data MoneyDim (crel :: CurrencyRelation)

instance Dimension (MoneyDim 'Base)

instance Dimension (MoneyDim 'Quote)

data MoneyBaseAmt = MoneyBaseAmt

instance Unit MoneyBaseAmt where
  type BaseUnit MoneyBaseAmt = Canonical
  type DimOfUnit MoneyBaseAmt = MoneyDim 'Base

instance Show MoneyBaseAmt where
  show = const "MoneyBaseAmt"

data MoneyQuoteAmt = MoneyQuoteAmt

instance Unit MoneyQuoteAmt where
  type BaseUnit MoneyQuoteAmt = Canonical
  type DimOfUnit MoneyQuoteAmt = MoneyDim 'Quote

instance Show MoneyQuoteAmt where
  show = const "MoneyQuoteAmt"

type LCSU' =
  MkLCSU
    '[ (MoneyDim 'Base, MoneyBaseAmt),
       (MoneyDim 'Quote, MoneyQuoteAmt)
     ]

type MoneyBase' =
  MkQu_DLN (MoneyDim 'Base) LCSU' Rational

type MoneyQuote' =
  MkQu_DLN (MoneyDim 'Quote) LCSU' Rational

--
-- Money sugar
--

newtype
  Money
    (crel :: CurrencyRelation)
    (act :: ExchangeAction) = Money
  { unMoney ::
      MkQu_DLN (MoneyDim crel) LCSU' Rational
  }
  deriving newtype
    ( NFData
    )
  deriving stock
    ( Eq,
      Ord
    )

unMoney' ::
  forall crel.
  ( SingI crel
  ) =>
  MkQu_DLN (MoneyDim crel) LCSU' Rational ->
  Rational
unMoney' =
  case sing :: Sing crel of
    SBase -> (# MoneyBaseAmt)
    SQuote -> (# MoneyQuoteAmt)

instance
  forall crel act unit.
  ( SingI crel,
    SingI act,
    Show unit,
    Typeable unit,
    Lookup (MoneyDim crel) LCSU' ~ unit
  ) =>
  Prelude.Show (Money crel act)
  where
  show (Money x) =
    ( case sing :: Sing crel of
        SBase -> showIn x MoneyBaseAmt
        SQuote -> showIn x MoneyQuoteAmt
    )
      <> " "
      <> show (fromSing (sing :: Sing act))

instance
  ( SingI crel
  ) =>
  TryFrom Rational (Money crel act)
  where
  tryFrom raw = do
    amt <- roundMoney raw
    if from amt == raw
      then pure amt
      else Left $ TryFromException raw Nothing

instance
  ( SingI crel
  ) =>
  From (Money crel act) Rational
  where
  from =
    unMkMoney

deriving via
  Rational
  instance
    ( SingI crel,
      Typeable crel,
      Typeable act
    ) =>
    PersistFieldSql (Money crel act)

instance
  ( SingI crel,
    Typeable crel,
    Typeable act
  ) =>
  PersistField (Money crel act)
  where
  toPersistValue =
    PersistRational . from
  fromPersistValue raw =
    case raw of
      PersistRational x -> first (const failure) $ tryFrom x
      _ -> Left failure
    where
      failure =
        showType @(Money crel act)
          <> " PersistValue is invalid "
          <> show raw

instance
  ( SingI crel,
    Typeable crel,
    Typeable act
  ) =>
  FromJSON (Money crel act)
  where
  parseJSON = A.withText
    (showType @(Money crel act))
    $ \raw -> do
      case tryReadViaRatio @Rational raw of
        Left x -> fail $ show x
        Right x -> pure x

--
-- SomeMoney sugar
--

data SomeMoney crel
  = forall act.
    ( SingI act,
      Show (Money crel act)
    ) =>
    SomeMoney
      (Sing act)
      (Money crel act)

instance Eq (SomeMoney crel) where
  (SomeMoney sx x) == (SomeMoney sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show (SomeMoney crel)

instance
  ( SingI crel,
    Show unit,
    Typeable unit,
    Lookup (MoneyDim crel) LCSU' ~ unit
  ) =>
  TryFrom Rational (SomeMoney crel)
  where
  tryFrom raw
    | raw > 0 && rounded == raw =
        Right
          $ SomeMoney (sing :: Sing 'Buy)
          $ mkMoney rounded
    | raw < 0 && rounded == raw =
        Right
          $ SomeMoney (sing :: Sing 'Sell)
          $ mkMoney rounded
    | otherwise =
        Left
          $ TryFromException raw Nothing
    where
      rounded =
        roundMoneyRat raw

-- | This dumb constructor is lossy, unsafe
-- and should not be exposed.
mkMoney ::
  forall crel act.
  ( SingI crel
  ) =>
  Rational ->
  Money crel act
mkMoney x =
  case sing :: Sing crel of
    SBase -> Money $ quOf (abs x) MoneyBaseAmt
    SQuote -> Money $ quOf (abs x) MoneyQuoteAmt

-- | This accessor is safe, but should not be exposed
-- because there is 'From' instance which is doing
-- the same thing.
unMkMoney ::
  forall crel act.
  ( SingI crel
  ) =>
  Money crel act ->
  Rational
unMkMoney =
  unMoney' @crel
    . unMoney

--
-- QuotePerBase sugar
--

type QuotePerBase' = MoneyQuote' %/ MoneyBase'

newtype QuotePerBase (act :: ExchangeAction) = QuotePerBase
  { unQuotePerBase :: QuotePerBase'
  }
  deriving newtype
    ( NFData
    )
  deriving stock
    ( Eq,
      Ord
    )

unQuotePerBase' ::
  QuotePerBase' ->
  Rational
unQuotePerBase' =
  (# quotePerBaseAmt)

mkQuotePerBase' ::
  Rational ->
  QuotePerBase'
mkQuotePerBase' =
  (% quotePerBaseAmt)

unUnitless ::
  MkQu_DLN Dimensionless LCSU' Rational ->
  Rational
unUnitless =
  (# Number)

quotePerBaseAmt :: MoneyQuoteAmt :/ MoneyBaseAmt
quotePerBaseAmt =
  MoneyQuoteAmt :/ MoneyBaseAmt

instance (SingI act) => Prelude.Show (QuotePerBase act) where
  show (QuotePerBase x) =
    showIn x quotePerBaseAmt
      <> " "
      <> show (fromSing (sing :: Sing act))

instance TryFrom Rational (QuotePerBase act) where
  tryFrom raw = do
    rate <- roundQuotePerBase raw
    if from rate == raw
      then pure rate
      else Left $ TryFromException raw Nothing

instance From (QuotePerBase act) Rational where
  from =
    (# quotePerBaseAmt) . unQuotePerBase

deriving via
  Rational
  instance
    ( Typeable act
    ) =>
    PersistFieldSql (QuotePerBase act)

instance
  ( Typeable act
  ) =>
  PersistField (QuotePerBase act)
  where
  toPersistValue =
    PersistRational . from
  fromPersistValue raw =
    case raw of
      PersistRational x -> first (const failure) $ tryFrom x
      _ -> Left failure
    where
      failure =
        showType @(QuotePerBase act)
          <> " PersistValue is invalid "
          <> show raw

--
-- SomeQuotePerBase sugar
--

data SomeQuotePerBase :: Type where
  SomeQuotePerBase ::
    ( SingI act
    ) =>
    Sing act ->
    QuotePerBase act ->
    SomeQuotePerBase

instance Eq SomeQuotePerBase where
  (SomeQuotePerBase sx x) == (SomeQuotePerBase sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

deriving stock instance Show SomeQuotePerBase

roundMoney ::
  forall crel act.
  ( SingI crel
  ) =>
  Rational ->
  Either
    (TryFromException Rational (Money crel act))
    (Money crel act)
roundMoney raw =
  if raw >= 0 && rounded >= 0
    then Right $ mkMoney rounded
    else Left $ TryFromException raw Nothing
  where
    rounded =
      roundMoneyRat raw

roundQuotePerBase ::
  forall act.
  Rational ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
roundQuotePerBase raw =
  if raw > 0 && rounded > 0
    then Right . QuotePerBase $ quOf rounded quotePerBaseAmt
    else Left $ TryFromException raw Nothing
  where
    rounded =
      roundQuotePerBaseRat raw

roundMoney' ::
  forall crel act.
  ( SingI crel
  ) =>
  MkQu_DLN (MoneyDim crel) LCSU' Rational ->
  Either
    (TryFromException Rational (Money crel act))
    (Money crel act)
roundMoney' =
  roundMoney
    . unMoney' @crel

roundQuotePerBase' ::
  forall act.
  QuotePerBase' ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
roundQuotePerBase' =
  roundQuotePerBase
    . unQuotePerBase'

roundMoneyRat :: Rational -> Rational
roundMoneyRat =
  dpRound 8

roundQuotePerBaseRat :: Rational -> Rational
roundQuotePerBaseRat =
  sdRound 5
    . dpRound 8

instance
  ( SingI act
  ) =>
  ToRequestParam (Money 'Base act)
  where
  toTextParam amt =
    case sing :: Sing act of
      SBuy -> toTextParam $ success amt
      SSell -> toTextParam $ (-1) * success amt
    where
      success :: Money 'Base act -> Rational
      success = abs . unMkMoney

instance ToRequestParam (QuotePerBase act) where
  toTextParam =
    toTextParam
      . from @(QuotePerBase act) @Rational

moneyBaseBuy :: QuasiQuoter
moneyBaseBuy =
  moneyQQ @'Base @'Buy

moneyBaseSell :: QuasiQuoter
moneyBaseSell =
  moneyQQ @'Base @'Sell

moneyQuoteBuy :: QuasiQuoter
moneyQuoteBuy =
  moneyQQ @'Quote @'Buy

moneyQuoteSell :: QuasiQuoter
moneyQuoteSell =
  moneyQQ @'Quote @'Sell

moneyQQ ::
  forall (crel :: CurrencyRelation) (act :: ExchangeAction).
  ( SingI crel,
    SingI act,
    Typeable crel,
    Typeable act
  ) =>
  QuasiQuoter
moneyQQ =
  QuasiQuoter
    { quoteDec = failure "quoteDec",
      quoteType = failure "quoteType",
      quotePat = failure "quotePat",
      quoteExp =
        \raw ->
          case tryReadViaRatio
            @Rational
            @(Money crel act)
            raw of
            Left e -> fail $ show e
            Right x -> do
              expr <- [e|mkMoney $(TH.lift $ unMkMoney x)|]
              case (sing :: Sing crel, sing :: Sing act) of
                (SBase, SBuy) -> do
                  TH.SigE expr
                    <$> [t|Money 'Base 'Buy|]
                (SBase, SSell) -> do
                  TH.SigE expr
                    <$> [t|Money 'Base 'Sell|]
                (SQuote, SBuy) -> do
                  TH.SigE expr
                    <$> [t|Money 'Quote 'Buy|]
                (SQuote, SSell) -> do
                  TH.SigE expr
                    <$> [t|Money 'Quote 'Sell|]
    }
  where
    failure :: Text -> a
    failure field =
      error
        $ showType @(Money crel act)
        <> " "
        <> field
        <> " is not implemented"

quotePerBaseBuy :: QuasiQuoter
quotePerBaseBuy =
  quotePerBaseQQ @'Buy

quotePerBaseSell :: QuasiQuoter
quotePerBaseSell =
  quotePerBaseQQ @'Sell

quotePerBaseQQ ::
  forall (act :: ExchangeAction).
  ( SingI act,
    Typeable act
  ) =>
  QuasiQuoter
quotePerBaseQQ =
  QuasiQuoter
    { quoteDec = failure "quoteDec",
      quoteType = failure "quoteType",
      quotePat = failure "quotePat",
      quoteExp =
        \raw ->
          case tryReadViaRatio
            @Rational
            @(QuotePerBase act)
            raw of
            Left e -> fail $ show e
            Right (QuotePerBase x) -> do
              expr <-
                [e|
                  QuotePerBase
                    $ $(TH.lift $ x # quotePerBaseAmt)
                    % quotePerBaseAmt
                  |]
              case sing :: Sing act of
                SBuy -> do
                  TH.SigE expr
                    <$> [t|QuotePerBase 'Buy|]
                SSell -> do
                  TH.SigE expr
                    <$> [t|QuotePerBase 'Sell|]
    }
  where
    failure :: Text -> a
    failure field =
      error
        $ showType @(QuotePerBase act)
        <> " "
        <> field
        <> " is not implemented"
