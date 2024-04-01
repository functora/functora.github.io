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
import Bfx.Data.Type
import Bfx.Import.External

tweakMoneyPip ::
  forall tags bos m.
  ( CashTags tags,
    GetTag 'Base tags,
    GetTag (bos :: BuyOrSell) tags,
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
    pip = newMoney 0.00000001

tweakMoneyPip' ::
  ( CashTags tags,
    GetTag 'Base tags,
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
    GetTag (bos :: BuyOrSell) tags,
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
    next = newMoney @tags $ unMoney @tags prev * tweak

newCounterOrder ::
  ( MonadThrow m
  ) =>
  Money (Tags 'Unsigned |+| 'Base |+| 'Buy |+| 'Gross) ->
  Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Buy |+| 'Net) ->
  Money (Tags 'Unsigned |+| 'Base |+| 'FeeRate) ->
  Money (Tags 'Unsigned |+| 'Quote |+| 'FeeRate) ->
  Money (Tags 'Unsigned |+| 'Quote |+| 'Buy |+| 'Net |+| 'ProfitRate) ->
  m
    ( Money (Tags 'Unsigned |+| 'Quote |+| 'Sell |+| 'Gross |+| 'Revenue),
      Money (Tags 'Unsigned |+| 'Base |+| 'Sell |+| 'Net),
      Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell)
    )
newCounterOrder enterBaseGain enterRate enterFee exitFee profRate = do
  exitQuote <- roundMoney exitQuoteGain
  exitBase <- tweakMoneyPip =<< roundMoney exitBaseLoss
  exitPrice <- roundQuotePerBase exitRate
  pure (exitQuote, exitBase, exitPrice)
  where
    exitBaseLoss :: Money (Tags 'Unsigned |+| 'Base |+| 'Sell |+| 'Net)
    exitBaseLoss =
      deductFee enterFee
        $ reTagMoney @'Buy @'Sell enterBaseGain
    enterQuoteLoss :: Money (Tags 'Unsigned |+| 'Quote |+| 'Buy |+| 'Net)
    enterQuoteLoss =
      exchangeMoney
        @(Tags 'Unsigned |+| 'Buy |+| 'Net)
        enterRate
        $ reTagMoney @'Gross @'Net
          enterBaseGain
    exitQuoteGain ::
      Money (Tags 'Unsigned |+| 'Quote |+| 'Sell |+| 'Gross |+| 'Revenue)
    exitQuoteGain =
      addFee exitFee
        . reTagMoney @'Buy @'Sell
        $ addProfit profRate enterQuoteLoss
    exitRate :: Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell)
    exitRate =
      newQuotePerBase @(Tags 'Unsigned |+| 'Sell)
        (unTagMoney @'Gross $ unTagMoney @'Revenue exitQuoteGain)
        (unTagMoney @'Net exitBaseLoss)

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
--       unMoney base
--     exitQuoteGain :: Money (Tags 'Unsigned |+| 'Quote |+| 'Net)
--     exitQuoteGain =
--       (exitBaseLoss |*| exitRate) |* (1 - exitFee)
