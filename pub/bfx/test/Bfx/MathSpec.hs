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
spec =
  it "newCounterOrder" $ do
    let rates =
          CounterRates
            { counterRatesEnterBaseFee = FeeRate 0.001,
              counterRatesExitQuoteFee = FeeRate 0.001,
              counterRatesExitQuoteProfit = ProfitRate 0.01
            }
    let args =
          CounterArgs
            { counterArgsEnterGrossBaseGain = MoneyAmount 0.2,
              counterArgsEnterQuotePerBase = QuotePerBase 5,
              counterArgsRates = rates
            }
    exit <- newCounterOrder args
    exit
      `shouldBe` CounterExit
        { -- Deduct fee and pip:
          -- 0.2 * 0.999 - 0.00000001
          counterExitNetBaseLoss = MoneyAmount 0.19979999,
          -- Add profit and fee:
          -- ((0.2 * 5) * (1 + 0.01) / (1 - 0.001)) / 0.19979999
          counterExitQuotePerBase = QuotePerBase 5.0601
        }
