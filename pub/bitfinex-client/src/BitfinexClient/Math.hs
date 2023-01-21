{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Math
  ( addFee,
    deductFee,
    tweakMoneyPip,
    tweakMakerRate,
    newCounterOrder,
    newCounterOrderSimple,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Metro
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

addFee ::
  forall crel act mrel.
  ( SingI crel
  ) =>
  Money crel act ->
  FeeRate mrel crel ->
  Either
    (TryFromException Rational (Money crel act))
    (Money crel act)
addFee amt fee =
  roundMoney' @crel $
    unMoney amt |/ (1 - unFeeRate fee)

deductFee ::
  forall crel act mrel.
  ( SingI crel
  ) =>
  Money crel act ->
  FeeRate mrel crel ->
  Either
    (TryFromException Rational (Money crel act))
    (Money crel act)
deductFee amt fee =
  roundMoney' @crel $
    unMoney amt |* (1 - unFeeRate fee)

tweakMoneyPip ::
  forall act.
  ( SingI act
  ) =>
  Money 'Base act ->
  Either
    (TryFromException Rational (Money 'Base act))
    (Money 'Base act)
tweakMoneyPip amt =
  case sing :: Sing act of
    SBuy -> tweakMoneyPip' (|+| pip) amt
    SSell -> tweakMoneyPip' (|-| pip) amt
  where
    pip :: MoneyBase'
    pip = unMoney [moneyBaseBuy|0.00000001|]

tweakMoneyPip' ::
  (MoneyBase' -> MoneyBase') ->
  Money 'Base act ->
  Either
    (TryFromException Rational (Money 'Base act))
    (Money 'Base act)
tweakMoneyPip' expr amt = do
  newAmt <-
    roundMoney'
      . expr
      $ unMoney amt
  if newAmt /= amt
    then pure newAmt
    else tweakMoneyPip' (expr . expr) amt

tweakMakerRate ::
  forall act.
  ( SingI act
  ) =>
  QuotePerBase act ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
tweakMakerRate rate =
  tweakMakerRateRec rate (unQuotePerBase rate) tweak
  where
    --
    -- TODO : use pip when 'units' bug with
    -- arithmetic underflow will be fixed.
    -- This implementation is wrong for
    -- non-negative types:
    --
    -- (|-|) :: (d1 @~ d2, Num n) => Qu d1 l n -> Qu d2 l n -> Qu d1 l n
    -- a |-| b = a |+| qNegate b
    --
    tweak :: Rational
    tweak =
      case sing :: Sing act of
        SBuy -> 999 % 1000
        SSell -> 1001 % 1000

tweakMakerRateRec ::
  QuotePerBase act ->
  QuotePerBase' ->
  Rational ->
  Either
    (TryFromException Rational (QuotePerBase act))
    (QuotePerBase act)
tweakMakerRateRec rate rate' tweak =
  case roundQuotePerBase' newRate' of
    Left e -> Left e
    Right x | x /= rate -> Right x
    Right {} -> tweakMakerRateRec rate newRate' tweak
  where
    newRate' = rate' |* tweak

newCounterOrder ::
  Money 'Base 'Buy ->
  QuotePerBase 'Buy ->
  FeeRate mrel0 'Base ->
  FeeRate mrel1 'Quote ->
  ProfitRate ->
  Either
    Error
    ( Money 'Quote 'Sell,
      Money 'Base 'Sell,
      QuotePerBase 'Sell
    )
newCounterOrder base0 rate0 feeB feeQ prof0 = do
  exitQuote <- tryErrorE $ roundMoney' exitQuoteGain
  exitBase0 <- tryErrorE $ roundMoney' exitBaseLoss
  exitBase <- tryErrorE $ tweakMoneyPip exitBase0
  exitPrice <- tryErrorE $ roundQuotePerBase' exitRate
  pure (exitQuote, exitBase, exitPrice)
  where
    enterFee :: Rational
    enterFee =
      from feeB
    exitFee :: Rational
    exitFee =
      from feeQ
    prof :: Rational
    prof =
      from prof0
    enterBaseGain :: MoneyBase'
    enterBaseGain =
      unMoney base0
    exitBaseLoss :: MoneyBase'
    exitBaseLoss =
      enterBaseGain |* (1 - enterFee)
    enterQuoteLoss :: MoneyQuote'
    enterQuoteLoss =
      enterBaseGain |*| unQuotePerBase rate0
    exitQuoteGain :: MoneyQuote'
    exitQuoteGain =
      (enterQuoteLoss |* (1 + prof)) |/ (1 - exitFee)
    exitRate :: QuotePerBase'
    exitRate =
      exitQuoteGain |/| exitBaseLoss

newCounterOrderSimple ::
  Money 'Base 'Sell ->
  QuotePerBase 'Sell ->
  FeeRate mrel 'Quote ->
  Either Error (Money 'Quote 'Sell)
newCounterOrderSimple base rate fee =
  tryErrorE $ roundMoney' exitQuoteGain
  where
    exitFee :: Rational
    exitFee =
      from fee
    exitRate :: QuotePerBase'
    exitRate =
      unQuotePerBase rate
    exitBaseLoss :: MoneyBase'
    exitBaseLoss =
      unMoney base
    exitQuoteGain :: MoneyQuote'
    exitQuoteGain =
      (exitBaseLoss |*| exitRate) |* (1 - exitFee)
