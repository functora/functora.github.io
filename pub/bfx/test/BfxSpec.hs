{-# OPTIONS_HADDOCK show-extensions #-}

module BfxSpec
  ( spec,
  )
where

import qualified Bfx
import qualified Bfx.Data.Candles as Candles
import qualified Bfx.Data.GetOrders as GetOrders
import qualified Bfx.Data.MarketAveragePrice as MarketAveragePrice
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.Import
import Bfx.TestEnv
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = before sysEnv $ do
  it "platformStatus succeeds" . const $ do
    ss <- Bfx.platformStatus
    ss `shouldBe` PltOperative
  it "symbolsDetails succeeds" . const $ do
    ss <- Bfx.symbolsDetails
    Map.lookup adaBtc ss
      `shouldBe` Just
        CurrencyPairConf
          { currencyPairPrecision = 5,
            currencyPairInitMargin = 30 % 1,
            currencyPairMinMargin = 15,
            currencyPairMaxOrderBaseAmt = MoneyAmount 250000,
            currencyPairMinOrderBaseAmt = MoneyAmount 4
          }
  it "marketAveragePrice succeeds" . const $ do
    buyRate <-
      Bfx.marketAveragePrice
        MarketAveragePrice.Request
          { MarketAveragePrice.buyOrSell = Buy,
            MarketAveragePrice.baseAmount = testAdaAmt,
            MarketAveragePrice.symbol = adaBtc
          }
    sellRate <-
      Bfx.marketAveragePrice
        MarketAveragePrice.Request
          { MarketAveragePrice.buyOrSell = Sell,
            MarketAveragePrice.baseAmount = testAdaAmt,
            MarketAveragePrice.symbol = adaBtc
          }
    buyRate `shouldSatisfy` (> sellRate)
  it "marketAveragePrice fails" . const $ do
    res <-
      tryAny
        $ Bfx.marketAveragePrice
          MarketAveragePrice.Request
            { MarketAveragePrice.buyOrSell = Sell,
              MarketAveragePrice.baseAmount = testAdaAmt,
              MarketAveragePrice.symbol = btcAda
            }
    res `shouldSatisfy` isLeft
  it "feeSummary succeeds" $ \env -> do
    res <- tryAny $ Bfx.feeSummary env
    res `shouldSatisfy` isRight
  it "submitOrderMaker and cancelOrderById succeeds" $ \env -> do
    curRate <-
      Bfx.marketAveragePrice
        MarketAveragePrice.Request
          { MarketAveragePrice.buyOrSell = Buy,
            MarketAveragePrice.baseAmount = testAdaAmt,
            MarketAveragePrice.symbol = adaBtc
          }
    rate <-
      roundQuotePerBase
        . QuotePerBase
        . (* 0.5)
        $ unQuotePerBase curRate
    order <-
      Bfx.submitOrderMaker
        env
        SubmitOrder.Request
          { SubmitOrder.buyOrSell = Buy,
            SubmitOrder.baseAmount = testAdaAmt,
            SubmitOrder.symbol = adaBtc,
            SubmitOrder.rate = rate,
            SubmitOrder.options = SubmitOrder.optsPostOnly
          }
    res <-
      tryAny
        . Bfx.cancelOrderById env
        $ orderId order
    res `shouldSatisfy` isRight
  it "retrieveOrders succeeds" $ \env -> do
    res <- tryAny . Bfx.retrieveOrders env $ GetOrders.optsSym adaBtc
    res `shouldSatisfy` isRight
  it "ordersHistory succeeds" $ \env -> do
    res <- tryAny . Bfx.ordersHistory env $ GetOrders.optsSym adaBtc
    res `shouldSatisfy` isRight
  it "getOrders succeeds" $ \env -> do
    res <- tryAny . Bfx.getOrders env $ GetOrders.optsSym adaBtc
    res `shouldSatisfy` isRight
  it "getOrder fails" $ \env -> do
    res <- tryAny . Bfx.getOrder env $ OrderId 0
    res `shouldSatisfy` isLeft
  it "submitCounterOrderMaker fails" $ \env -> do
    res <-
      tryAny
        $ Bfx.submitCounterOrderMaker
          env
          (OrderId 0)
          CounterRates
            { counterRatesEnterBaseFee = FeeRate 0,
              counterRatesExitQuoteFee = FeeRate 0,
              counterRatesExitQuoteProfit = ProfitRate 0
            }
          SubmitOrder.optsPostOnly
    res `shouldSatisfy` isLeft
  it "wallets succeeds" $ \env -> do
    res <- tryAny $ Bfx.wallets env
    res `shouldSatisfy` isRight
  it "netWorth succeeds" $ \env -> do
    res <- tryAny . Bfx.netWorth env $ CurrencyCode "BTC"
    res `shouldSatisfy` isRight
  it "candlesLast succeeds" . const $ do
    res <- tryAny $ Bfx.candlesLast Ctf1h adaBtc Candles.optsDef
    res `shouldSatisfy` isRight
  it "candlesHist succeeds" . const $ do
    res <- tryAny $ Bfx.candlesHist Ctf1h adaBtc Candles.optsDef
    res `shouldSatisfy` isRight

--  describe "End2End" $ do
--    itRight "submitOrderMaker" $ \env -> do
--      let amt = from @(Ratio Natural) 2.002002 :: Money 'Base 'Buy
--      let sym = [currencyPair|ADABTC|]
--      let opts = SubmitOrder.optsPostOnly
--      rate <- Bfx.marketAveragePrice amt sym
--      Bfx.submitOrderMaker env amt sym rate opts
--    itRight "submitCounterOrderMaker" $ \env ->
--      Bfx.submitCounterOrderMaker
--        env
--        (OrderId 0)
--        [feeRateMakerBase| 0.001 |]
--        [feeRateMakerQuote| 0.001 |]
--        [profitRate| 0.001 |]
--        SubmitOrder.optsPostOnly
-- focus . itRight "cancelOrderMulti" $ \env ->
--   Bfx.cancelOrderMulti
--     env
--     CancelOrderMulti.Everything
-- focus . itRight "dumpIntoQuoteMaker" $ \env ->
--   Bfx.dumpIntoQuoteMaker
--     env
--     [currencyPair|XLM:BTC|]
--     SubmitOrder.optsPostOnly
