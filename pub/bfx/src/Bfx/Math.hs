{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Math
  ( tweakMoneyAmount,
    tweakQuotePerBase,
    newCounterOrder,
    CounterArgs (..),
    CounterRates (..),
    CounterExit (..),
    roundMoneyAmount,
    roundQuotePerBase,
  )
where

import Bfx.Import.External

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

tweakQuotePerBase ::
  ( MonadThrow m
  ) =>
  BuyOrSell ->
  QuotePerBase ->
  m QuotePerBase
tweakQuotePerBase =
  tweakQuotePerBaseRec pip

tweakQuotePerBaseRec ::
  ( MonadThrow m
  ) =>
  Ratio Natural ->
  BuyOrSell ->
  QuotePerBase ->
  m QuotePerBase
tweakQuotePerBaseRec tweak bos prev = do
  next <- roundQuotePerBase
    . QuotePerBase
    $ case bos of
      Buy -> unQuotePerBase prev - tweak
      Sell -> unQuotePerBase prev + tweak
  if next /= prev
    then pure next
    else tweakQuotePerBaseRec (tweak + pip) bos prev

pip :: Ratio Natural
pip = 0.00000001

data CounterArgs = CounterArgs
  { counterArgsEnterGrossBaseGain :: MoneyAmount,
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
  { counterRatesEnterBaseFee :: FeeRate,
    counterRatesExitQuoteFee :: FeeRate,
    counterRatesExitQuoteProfit :: ProfitRate
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
  { counterExitNetBaseLoss :: MoneyAmount,
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

newCounterOrder :: (MonadThrow m) => CounterArgs -> m CounterExit
newCounterOrder args = do
  exitBase <- tweakMoneyAmount Sell exitBaseLoss
  exitPrice <- roundQuotePerBase exitRate
  pure
    CounterExit
      { counterExitNetBaseLoss = exitBase,
        counterExitQuotePerBase = exitPrice
      }
  where
    enterBaseGain :: MoneyAmount
    enterBaseGain =
      counterArgsEnterGrossBaseGain args
    enterRate :: QuotePerBase
    enterRate =
      counterArgsEnterQuotePerBase args
    enterFee :: FeeRate
    enterFee =
      counterRatesEnterBaseFee $ counterArgsRates args
    exitFee :: FeeRate
    exitFee =
      counterRatesExitQuoteFee $ counterArgsRates args
    profRate :: ProfitRate
    profRate =
      counterRatesExitQuoteProfit $ counterArgsRates args
    exitBaseLoss :: MoneyAmount
    exitBaseLoss =
      MoneyAmount $ unMoneyAmount enterBaseGain * (1 - unFeeRate enterFee)
    enterQuoteLoss :: MoneyAmount
    enterQuoteLoss =
      MoneyAmount $ unMoneyAmount enterBaseGain * unQuotePerBase enterRate
    exitQuoteGain :: MoneyAmount
    exitQuoteGain =
      MoneyAmount
        $ (unMoneyAmount enterQuoteLoss * (1 + unProfitRate profRate))
        / (1 - unFeeRate exitFee)
    exitRate :: QuotePerBase
    exitRate =
      QuotePerBase
        $ unMoneyAmount exitQuoteGain
        / unMoneyAmount exitBaseLoss

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
