{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Math
  ( tweakMoneyPip,
    tweakMakerRate,
    newCounterOrder,
    CounterArgs (..),
    CounterExit (..),
    -- newCounterOrderSimple,
  )
where

import Bfx.Data.Metro
import Bfx.Import.External

tweakMoneyPip ::
  ( MonadThrow m
  ) =>
  BuyOrSell ->
  MoneyAmount ->
  m MoneyAmount
tweakMoneyPip =
  tweakMoneyPipRec pip

tweakMoneyPipRec ::
  ( MonadThrow m
  ) =>
  Ratio Natural ->
  BuyOrSell ->
  MoneyAmount ->
  m MoneyAmount
tweakMoneyPipRec tweak bos prev = do
  next <-
    roundMoneyAmount
      . MoneyAmount
      $ case bos of
        Buy -> unMoneyAmount prev + tweak
        Sell -> unMoneyAmount prev - tweak
  if next /= prev
    then pure next
    else tweakMoneyPipRec (tweak + pip) bos prev

tweakMakerRate ::
  ( MonadThrow m
  ) =>
  BuyOrSell ->
  QuotePerBase ->
  m QuotePerBase
tweakMakerRate =
  tweakMakerRateRec pip

tweakMakerRateRec ::
  ( MonadThrow m
  ) =>
  Ratio Natural ->
  BuyOrSell ->
  QuotePerBase ->
  m QuotePerBase
tweakMakerRateRec tweak bos prev = do
  next <- roundQuotePerBase
    . QuotePerBase
    $ case bos of
      Buy -> unQuotePerBase prev - tweak
      Sell -> unQuotePerBase prev + tweak
  if next /= prev
    then pure next
    else tweakMakerRateRec (tweak + pip) bos prev

pip :: Ratio Natural
pip = 0.00000001

data CounterArgs = CounterArgs
  { counterArgsEnterGrossBaseGain :: MoneyAmount,
    counterArgsEnterQuotePerBase :: QuotePerBase,
    counterArgsEnterBaseFeeRate :: FeeRate,
    counterArgsExitQuoteFeeRate :: FeeRate,
    counterArgsExitQuoteProfitRate :: ProfitRate
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
  exitBase <- tweakMoneyPip Sell =<< roundMoneyAmount exitBaseLoss
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
      counterArgsEnterBaseFeeRate args
    exitFee :: FeeRate
    exitFee =
      counterArgsExitQuoteFeeRate args
    profRate :: ProfitRate
    profRate =
      counterArgsExitQuoteProfitRate args
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
