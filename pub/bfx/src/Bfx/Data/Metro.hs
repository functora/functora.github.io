{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Metro
  ( CashTags,
    roundMoney,
    RateTags,
    roundQuotePerBase,
  )
where

import Bfx.Import.External

type CashTags tags =
  ( MoneyTags tags,
    HasTag 'Unsigned tags
  )

roundMoney ::
  forall tags m.
  ( CashTags tags,
    MonadThrow m
  ) =>
  Money tags ->
  m (Money tags)
roundMoney money =
  if raw >= 0 && rounded >= 0
    then pure $ newMoney rounded
    else throw $ TryFromException @(Ratio Natural) @(Money tags) raw Nothing
  where
    raw = unMoney money
    rounded =
      unsafeFrom @Rational @(Ratio Natural)
        . roundMoneyRat
        $ from @(Ratio Natural) @Rational raw

type RateTags tags =
  ( MoneyTags tags,
    HasTag 'Unsigned tags,
    HasTag 'QuotePerBase tags
  )

roundQuotePerBase ::
  forall tags m.
  ( RateTags tags,
    MonadThrow m
  ) =>
  Money tags ->
  m (Money tags)
roundQuotePerBase money =
  if raw > 0 && rounded > 0
    then pure $ newMoney rounded
    else throw $ TryFromException @(Ratio Natural) @(Money tags) raw Nothing
  where
    raw = unMoney money
    rounded =
      unsafeFrom @Rational @(Ratio Natural)
        . roundQuotePerBaseRat
        $ from @(Ratio Natural) @Rational raw

roundMoneyRat :: Rational -> Rational
roundMoneyRat = dpRound 8

roundQuotePerBaseRat :: Rational -> Rational
roundQuotePerBaseRat = sdRound 5 . dpRound 8
