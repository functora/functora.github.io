{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.MathSpec
  ( spec,
  )
where

import Bfx
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.TestEnv
import Functora.Money
import Functora.Prelude
import Test.Hspec

spec :: Spec
spec = do
  it "mkCounterOrder/Buy" $ do
    let args =
          MkCounterOrder
            { mkCounterOrderEnterBuyOrSell = Buy,
              mkCounterOrderEnterGrossBase = MoneyAmount 0.2,
              -- Unrealistic high price to ignore current market state.
              mkCounterOrderEnterQuotePerBase = QuotePerBase 5,
              mkCounterOrderCurrencyPair = adaBtc,
              mkCounterOrderEnterFee = FeeRate 0.001,
              mkCounterOrderExitFee = FeeRate 0.001,
              mkCounterOrderProfit = ProfitRate 0.01
            }
    exit <- mkCounterOrder args
    exit
      `shouldBe` SubmitOrder.Request
        { SubmitOrder.buyOrSell = Sell,
          -- Deduct fee and pip:
          -- 0.2 * 0.999 - 0.00000001
          SubmitOrder.baseAmount = MoneyAmount 0.19979999,
          SubmitOrder.symbol = adaBtc,
          -- Add profit and fee:
          -- ((0.2 * 5) * (1 + 0.01) / (1 - 0.001)) / 0.19979999
          SubmitOrder.rate = QuotePerBase 5.0601,
          SubmitOrder.options = SubmitOrder.optsDef
        }
  it "mkCounterOrder/Sell" $ do
    let args =
          MkCounterOrder
            { mkCounterOrderEnterBuyOrSell = Sell,
              mkCounterOrderEnterGrossBase = MoneyAmount 0.2,
              -- Unrealistic low price to ignore current market state.
              mkCounterOrderEnterQuotePerBase = QuotePerBase 5,
              mkCounterOrderCurrencyPair = btcUsd,
              mkCounterOrderEnterFee = FeeRate 0.001,
              mkCounterOrderExitFee = FeeRate 0.001,
              mkCounterOrderProfit = ProfitRate 0.01
            }
    exit <- mkCounterOrder args
    exit
      `shouldBe` SubmitOrder.Request
        { SubmitOrder.buyOrSell = Buy,
          -- Add profit, deduct fee and add pip:
          -- ((0.2 * (1 + 0.01)) / (1 - 0.001)) + 0.00000001
          SubmitOrder.baseAmount = MoneyAmount 0.20220221,
          SubmitOrder.symbol = btcUsd,
          -- Deduct fee and divide by base:
          -- (0.2 * 5 * (1 - 0.001)) / 0.20220221
          SubmitOrder.rate = QuotePerBase 4.9406,
          SubmitOrder.options = SubmitOrder.optsDef
        }
