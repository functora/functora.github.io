{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Math
  ( roundMoneyAmount,
    roundQuotePerBase,
    tweakMoneyAmount,
    tweakQuotePerBase,
    CounterArgs (..),
    CounterRates (..),
    CounterExit (..),
    mkCounterOrder,
  )
where

import Data.Ratio.Rounding (dpRound, sdRound)
import Functora.Money
import Functora.Prelude

roundMoneyAmount :: (MonadThrow m) => MoneyAmount -> m MoneyAmount
roundMoneyAmount arg@(MoneyAmount raw) =
  if raw >= 0 && rounded >= 0
    then pure $ MoneyAmount rounded
    else throwString $ "Rounding error for " <> inspect @String arg
  where
    rounded =
      unsafeFrom @Rational @(Ratio Natural)
        . roundMoneyAmountRat
        $ from @(Ratio Natural) @Rational raw

roundQuotePerBase :: (MonadThrow m) => QuotePerBase -> m QuotePerBase
roundQuotePerBase arg@(QuotePerBase raw) =
  if raw > 0 && rounded > 0
    then pure $ QuotePerBase rounded
    else throwString $ "Rounding error for " <> inspect @String arg
  where
    rounded =
      unsafeFrom @Rational @(Ratio Natural)
        . roundQuotePerBaseRat
        $ from @(Ratio Natural) @Rational raw

roundMoneyAmountRat :: Rational -> Rational
roundMoneyAmountRat = dpRound 8

roundQuotePerBaseRat :: Rational -> Rational
roundQuotePerBaseRat = sdRound 5 . dpRound 8

tweakMoneyAmount ::
  ( MonadThrow m
  ) =>
  BuyOrSell ->
  MoneyAmount ->
  m MoneyAmount
tweakMoneyAmount =
  tweakMoneyAmountRec pip

tweakMoneyAmountRec ::
  ( MonadThrow m
  ) =>
  Ratio Natural ->
  BuyOrSell ->
  MoneyAmount ->
  m MoneyAmount
tweakMoneyAmountRec tweak bos prev = do
  next <-
    roundMoneyAmount
      . MoneyAmount
      $ case bos of
        Buy -> unMoneyAmount prev + tweak
        Sell -> unMoneyAmount prev - tweak
  if next /= prev
    then pure next
    else tweakMoneyAmountRec (tweak + pip) bos prev

pip :: Ratio Natural
pip = 0.00000001

tweakQuotePerBase ::
  ( MonadThrow m
  ) =>
  BuyOrSell ->
  QuotePerBase ->
  m QuotePerBase
tweakQuotePerBase bos rate =
  tweakQuotePerBaseRec rate (* tweak)
  where
    tweak :: Ratio Natural
    tweak =
      case bos of
        Buy -> 999 % 1000
        Sell -> 1001 % 1000

tweakQuotePerBaseRec ::
  ( MonadThrow m
  ) =>
  QuotePerBase ->
  (Ratio Natural -> Ratio Natural) ->
  m QuotePerBase
tweakQuotePerBaseRec prev tweak = do
  next <-
    roundQuotePerBase
      . QuotePerBase
      . tweak
      $ unQuotePerBase prev
  if next /= prev
    then pure next
    else tweakQuotePerBaseRec prev $ tweak . tweak

data CounterArgs = CounterArgs
  { counterArgsEnterBuyOrSell :: BuyOrSell,
    counterArgsEnterGrossBase :: MoneyAmount,
    counterArgsEnterQuotePerBase :: QuotePerBase,
    counterArgsRates :: CounterRates
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data CounterRates = CounterRates
  { counterRatesFee :: FeeRate,
    counterRatesProfit :: ProfitRate
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data CounterExit = CounterExit
  { counterExitBuyOrSell :: BuyOrSell,
    counterExitGrossBase :: MoneyAmount,
    counterExitQuotePerBase :: QuotePerBase
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

mkCounterOrder :: (MonadThrow m) => CounterArgs -> m CounterExit
mkCounterOrder args = do
  let exitBos = nextEnum enterBos
  exitBase <-
    tweakMoneyAmount exitBos exitGrossBase
  exitRate <-
    roundQuotePerBase
      . QuotePerBase
      $ unMoneyAmount exitGrossQuote
      / unMoneyAmount exitBase
  pure
    CounterExit
      { counterExitBuyOrSell = exitBos,
        counterExitGrossBase = exitBase,
        counterExitQuotePerBase = exitRate
      }
  where
    enterBos :: BuyOrSell
    enterBos = counterArgsEnterBuyOrSell args
    enterRate :: QuotePerBase
    enterRate = counterArgsEnterQuotePerBase args
    enterBase :: MoneyAmount
    enterBase = counterArgsEnterGrossBase args
    enterNetLoss :: MoneyAmount
    enterNetLoss =
      MoneyAmount $ case enterBos of
        Buy ->
          -- Quote
          unMoneyAmount enterBase
            * unQuotePerBase enterRate
        Sell ->
          -- Base
          unMoneyAmount enterBase
    enterNetGain :: MoneyAmount
    enterNetGain =
      MoneyAmount $ case enterBos of
        Buy ->
          -- Base
          unMoneyAmount enterBase
            * (1 - unFeeRate feeRate)
        Sell ->
          -- Quote
          unMoneyAmount enterBase
            * unQuotePerBase enterRate
            * (1 - unFeeRate feeRate)
    exitGrossGain :: MoneyAmount
    exitGrossGain =
      --
      -- Buy = Quote
      -- Sell = Base
      --
      MoneyAmount
        $ unMoneyAmount enterNetLoss
        * (1 + unProfitRate profitRate)
        / (1 - unFeeRate feeRate)
    exitGrossBase :: MoneyAmount
    exitGrossBase =
      case enterBos of
        Buy -> enterNetGain
        Sell -> exitGrossGain
    exitGrossQuote :: MoneyAmount
    exitGrossQuote =
      case enterBos of
        Buy -> exitGrossGain
        Sell -> enterNetGain
    feeRate :: FeeRate
    feeRate = counterRatesFee $ counterArgsRates args
    profitRate :: ProfitRate
    profitRate = counterRatesProfit $ counterArgsRates args
