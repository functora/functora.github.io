{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.MathSpec
  ( spec,
  )
where

import Bfx
import Functora.Money
import Functora.Prelude
import Test.Hspec

spec :: Spec
spec = do
  it "mkCounterOrder/Buy" $ do
    let rates =
          CounterRates
            { counterRatesFee = FeeRate 0.001,
              counterRatesProfit = ProfitRate 0.01
            }
    let args =
          CounterArgs
            { counterArgsEnterBuyOrSell = Buy,
              counterArgsEnterGrossBase = MoneyAmount 0.2,
              counterArgsEnterQuotePerBase = QuotePerBase 5,
              counterArgsRates = rates
            }
    exit <- mkCounterOrder args
    exit
      `shouldBe` CounterExit
        { counterExitBuyOrSell = Sell,
          -- Deduct fee and pip:
          -- 0.2 * 0.999 - 0.00000001
          counterExitGrossBase = MoneyAmount 0.19979999,
          -- Add profit and fee:
          -- ((0.2 * 5) * (1 + 0.01) / (1 - 0.001)) / 0.19979999
          counterExitQuotePerBase = QuotePerBase 5.0601
        }
  it "mkCounterOrder/Sell" $ do
    let rates =
          CounterRates
            { counterRatesFee = FeeRate 0.001,
              counterRatesProfit = ProfitRate 0.01
            }
    let args =
          CounterArgs
            { counterArgsEnterBuyOrSell = Sell,
              counterArgsEnterGrossBase = MoneyAmount 0.2,
              counterArgsEnterQuotePerBase = QuotePerBase 5,
              counterArgsRates = rates
            }
    exit <- mkCounterOrder args
    exit
      `shouldBe` CounterExit
        { counterExitBuyOrSell = Buy,
          -- Add profit, deduct fee and add pip:
          -- ((0.2 * (1 + 0.01)) / (1 - 0.001)) + 0.00000001
          counterExitGrossBase = MoneyAmount 0.20220221,
          -- Deduct fee and divide by base:
          -- (0.2 * 5 * (1 - 0.001)) / 0.20220221
          counterExitQuotePerBase = QuotePerBase 4.9406
        }
