{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Math
  ( roundMoneyAmount,
    roundQuotePerBase,
    tweakMoneyAmount,
    tweakQuotePerBase,
  )
where

import Functora.Money
import Functora.Prelude
import Functora.Round (dpRound, sdRound)

roundMoneyAmount :: (MonadThrow m) => MoneyAmount -> m MoneyAmount
roundMoneyAmount prev = do
  next <-
    either throw (pure . MoneyAmount)
      . tryFrom @Fix @FixNonNeg
      . dpRound 8
      . unFixNonNeg
      $ unMoneyAmount prev
  if prev >= 0 && next >= 0
    then pure next
    else throwString $ "Rounding error for " <> inspect @Text prev

roundQuotePerBase :: (MonadThrow m) => QuotePerBase -> m QuotePerBase
roundQuotePerBase prev = do
  next <-
    either throw (pure . QuotePerBase)
      . tryFrom @Fix @FixNonNeg
      . sdRound 5
      . dpRound 8
      . unFixNonNeg
      $ unQuotePerBase prev
  if prev > 0 && next > 0
    then pure next
    else throwString $ "Rounding error for " <> inspect @Text prev

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
  MoneyAmount ->
  BuyOrSell ->
  MoneyAmount ->
  m MoneyAmount
tweakMoneyAmountRec tweak bos prev = do
  next <-
    roundMoneyAmount $ case bos of
      Buy -> prev + tweak
      Sell -> prev - tweak
  if next /= prev
    then pure next
    else tweakMoneyAmountRec (tweak + pip) bos prev

pip :: MoneyAmount
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
    tweak :: QuotePerBase
    tweak =
      case bos of
        Buy -> 0.999
        Sell -> 1.001

tweakQuotePerBaseRec ::
  ( MonadThrow m
  ) =>
  QuotePerBase ->
  (QuotePerBase -> QuotePerBase) ->
  m QuotePerBase
tweakQuotePerBaseRec prev tweak = do
  next <- roundQuotePerBase $ tweak prev
  if next /= prev
    then pure next
    else tweakQuotePerBaseRec prev $ tweak . tweak
