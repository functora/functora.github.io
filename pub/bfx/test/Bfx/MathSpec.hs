{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.MathSpec
  ( spec,
  )
where

import Bfx.Import
import Test.Hspec

spec :: Spec
spec =
  focus . it "newCounterOrder" $ do
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
    --
    -- TODO : !!!
    --
    exit
      `shouldBe` CounterExit
        { counterExitNetBaseLoss = MoneyAmount 1,
          counterExitQuotePerBase = QuotePerBase 1
        }
