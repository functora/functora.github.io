{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Math
  ( tweakMoneyPip,
    tweakMakerRate,
    newCounterOrder,
    -- newCounterOrderSimple,
  )
where

import Bfx.Data.Kind
import Bfx.Data.Metro
import Bfx.Import.External

tweakMoneyPip ::
  forall tags bos m.
  ( CashTags tags,
    HasTag 'Base tags,
    HasTag (bos :: BuyOrSell) tags,
    MonadThrow m
  ) =>
  Money tags ->
  m (Money tags)
tweakMoneyPip amt =
  case sing :: Sing bos of
    SBuy -> tweakMoneyPip' (`addMoney` pip) amt
    SSell -> tweakMoneyPip' (`deductMoney` pip) amt
  where
    pip :: Money tags
    pip = Tagged 0.00000001

tweakMoneyPip' ::
  ( CashTags tags,
    HasTag 'Base tags,
    MonadThrow m
  ) =>
  (Money tags -> Money tags) ->
  Money tags ->
  m (Money tags)
tweakMoneyPip' expr amt = do
  newAmt <- roundMoney $ expr amt
  if newAmt /= amt
    then pure newAmt
    else tweakMoneyPip' (expr . expr) amt

tweakMakerRate ::
  forall tags bos m.
  ( RateTags tags,
    HasTag (bos :: BuyOrSell) tags,
    MonadThrow m
  ) =>
  Money tags ->
  m (Money tags)
tweakMakerRate rate =
  tweakMakerRateRec rate rate tweak
  where
    --
    -- TODO : ??? use pip when 'units' bug with
    -- arithmetic underflow will be fixed.
    -- This implementation is wrong for
    -- non-negative types:
    --
    -- (|-|) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
    -- a |-| b = a |+| qNegate b
    --
    tweak :: Ratio Natural
    tweak =
      case sing :: Sing bos of
        SBuy -> 999 % 1000
        SSell -> 1001 % 1000

tweakMakerRateRec ::
  forall tags m.
  ( RateTags tags,
    MonadThrow m
  ) =>
  Money tags ->
  Money tags ->
  Ratio Natural ->
  m (Money tags)
tweakMakerRateRec rate prev tweak =
  case roundQuotePerBase next of
    Left e -> throw e
    Right x | x /= rate -> pure x
    Right {} -> tweakMakerRateRec rate next tweak
  where
    next = Tagged @tags $ unTagged @tags prev * tweak

newCounterOrder ::
  ( MonadThrow m
  ) =>
  Money (Tags 'Unsigned |+| 'Base |+| 'Buy |+| 'Gross |+| 'MoneyAmount) ->
  Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Buy |+| 'Net) ->
  Money (Tags 'Unsigned |+| 'Base |+| 'FeeRate) ->
  Money (Tags 'Unsigned |+| 'Quote |+| 'FeeRate) ->
  Money (Tags 'Unsigned |+| 'Quote |+| 'Buy |+| 'Net |+| 'ProfitRate) ->
  m
    ( Money
        ( Tags 'Unsigned
            |+| 'Quote
            |+| 'Sell
            |+| 'Gross
            |+| 'Revenue
            |+| 'MoneyAmount
        ),
      Money
        ( Tags 'Unsigned
            |+| 'Base
            |+| 'Sell
            |+| 'Net
            |+| 'MoneyAmount
        ),
      Money
        ( Tags 'Unsigned
            |+| 'QuotePerBase
            |+| 'Sell
        )
    )
newCounterOrder enterBaseGain enterRate enterFee exitFee profRate = do
  exitQuote <- roundMoney exitQuoteGain
  exitBase <-
    tweakMoneyPip
      =<< roundMoney exitBaseLoss
  exitPrice <- roundQuotePerBase exitRate
  pure (exitQuote, exitBase, exitPrice)
  where
    exitBaseLoss ::
      Money (Tags 'Unsigned |+| 'Base |+| 'Sell |+| 'Net |+| 'MoneyAmount)
    exitBaseLoss =
      deductFee enterFee
        $ reTag @'Buy @'Sell enterBaseGain
    enterQuoteLoss ::
      Money (Tags 'Unsigned |+| 'Quote |+| 'Buy |+| 'Net |+| 'MoneyAmount)
    enterQuoteLoss =
      exchangeMoney @(Tags 'Unsigned |+| 'Buy |+| 'Net) enterRate
        $ reTag @'Gross @'Net enterBaseGain
    exitQuoteGain ::
      Money
        ( Tags 'Unsigned
            |+| 'Quote
            |+| 'Sell
            |+| 'Gross
            |+| 'Revenue
            |+| 'MoneyAmount
        )
    exitQuoteGain =
      addFee
        exitFee
        . reTag @'Buy @'Sell
        $ addProfit
          @(Tags 'Unsigned |+| 'Quote |+| 'Buy |+| 'Net)
          profRate
          enterQuoteLoss
    exitRate :: Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell)
    exitRate =
      newQuotePerBase @(Tags 'Unsigned |+| 'Sell |+| 'MoneyAmount)
        (unTag @'Gross $ unTag @'Revenue exitQuoteGain)
        (unTag @'Net exitBaseLoss)

-- newCounterOrderSimple ::
--   ( MonadThrow m
--   ) =>
--   Money (Tags 'Unsigned |+| 'Base |+| 'Sell) ->
--   Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell) ->
--   Money (Tags 'Unsigned |+| 'FeeRate |+| 'Quote) ->
--   m (Money (Tags 'Unsigned |+| 'Quote |+| 'Sell))
-- newCounterOrderSimple base rate fee =
--   tryErrorE $ roundMoney' exitQuoteGain
--   where
--     exitFee :: Rational
--     exitFee =
--       from fee
--     exitRate :: Money (Tags 'Unsigned |+| 'QuotePerBase)
--     exitRate =
--       unQuotePerBase rate
--     exitBaseLoss :: Money (Tags 'Unsigned |+| 'Base |+| 'Sell)
--     exitBaseLoss =
--       unTagged base
--     exitQuoteGain :: Money (Tags 'Unsigned |+| 'Quote |+| 'Net)
--     exitQuoteGain =
--       (exitBaseLoss |*| exitRate) |* (1 - exitFee)
