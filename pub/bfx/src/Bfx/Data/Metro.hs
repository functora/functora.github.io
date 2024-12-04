{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Metro
  ( roundMoneyAmount,
    roundQuotePerBase,
  )
where

import Bfx.Import.External

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
