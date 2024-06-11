{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BfxSpec
  ( spec,
  )
where

import qualified Bfx as Bitfinex
-- import qualified Bfx.Chart as Chart
-- import qualified Bfx.Data.CancelOrderMulti as CancelOrderMulti

import qualified Bfx.Data.Candles as Candles
import qualified Bfx.Data.GetOrders as GetOrders
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.Import
import Bfx.TestEnv
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = before sysEnv $ do
  let adabtc = either impureThrow id $ newCurrencyPair "ADABTC"
  it "platformStatus succeeds" . const $ do
    ss <- Bitfinex.platformStatus
    ss `shouldBe` PltOperative
  it "symbolsDetails succeeds" . const $ do
    ss <- Bitfinex.symbolsDetails
    Map.lookup adabtc ss
      `shouldBe` Just
        CurrencyPairConf
          { currencyPairPrecision = 5,
            currencyPairInitMargin = 30 % 1,
            currencyPairMinMargin = 15,
            currencyPairMaxOrderAmt = Tagged 250000,
            currencyPairMinOrderAmt = Tagged 4
          }
  it "marketAveragePrice succeeds" . const $ do
    sym <- newCurrencyPair "ADABTC"
    buyRate <- Bitfinex.marketAveragePrice (testAmt @'Buy) sym
    sellRate <- Bitfinex.marketAveragePrice (testAmt @'Sell) sym
    unTagged buyRate `shouldSatisfy` (> unTagged sellRate)
  it "marketAveragePrice fails" . const $ do
    let amt = testAmt @'Buy
    sym <- newCurrencyPair "BTCADA"
    res <- tryAny $ Bitfinex.marketAveragePrice amt sym
    res `shouldSatisfy` isLeft
  it "feeSummary succeeds" $ \env -> do
    res <- tryAny $ Bitfinex.feeSummary env
    res `shouldSatisfy` isRight
  it "submitOrderMaker and cancelOrderById succeeds" $ \env -> do
    let amt = testAmt @'Buy
    let opts = SubmitOrder.optsPostOnly
    sym <- newCurrencyPair "ADABTC"
    curRate <- Bitfinex.marketAveragePrice amt sym
    rate <- (* 0.5) <$> roundQuotePerBase curRate
    order <- Bitfinex.submitOrderMaker env amt sym rate opts
    res <- tryAny . Bitfinex.cancelOrderById env $ orderId order
    res `shouldSatisfy` isRight
  it "retrieveOrders succeeds" $ \env -> do
    res <- tryAny . Bitfinex.retrieveOrders env $ GetOrders.optsSym adabtc
    res `shouldSatisfy` isRight
  it "ordersHistory succeeds" $ \env -> do
    res <- tryAny . Bitfinex.ordersHistory env $ GetOrders.optsSym adabtc
    res `shouldSatisfy` isRight
  it "getOrders succeeds" $ \env -> do
    res <- tryAny . Bitfinex.getOrders env $ GetOrders.optsSym adabtc
    res `shouldSatisfy` isRight
  it "getOrder fails" $ \env -> do
    res <- tryAny . Bitfinex.getOrder env $ OrderId 0
    res `shouldSatisfy` isLeft
  it "submitCounterOrderMaker fails" $ \env -> do
    res <-
      tryAny
        $ Bitfinex.submitCounterOrderMaker
          env
          (OrderId 0)
          (Tagged 0.001)
          (Tagged 0.001)
          (Tagged 0.001)
          SubmitOrder.optsPostOnly
    res `shouldSatisfy` isLeft
  it "wallets succeeds" $ \env -> do
    res <- tryAny $ Bitfinex.wallets env
    res `shouldSatisfy` isRight
  it "netWorth succeeds" $ \env -> do
    res <- tryAny . Bitfinex.netWorth env $ CurrencyCode "BTC"
    res `shouldSatisfy` isRight
  it "candlesLast succeeds" . const $ do
    res <- tryAny $ Bitfinex.candlesLast Ctf1h adabtc Candles.optsDef
    res `shouldSatisfy` isRight
  it "candlesHist succeeds" . const $ do
    res <- tryAny $ Bitfinex.candlesHist Ctf1h adabtc Candles.optsDef
    res `shouldSatisfy` isRight

--  it "chart" . const $ do
--    Chart.newExample
--    True `shouldBe` True

--  describe "End2End" $ do
--    itRight "submitOrderMaker" $ \env -> do
--      let amt = from @(Ratio Natural) 2.002002 :: Money 'Base 'Buy
--      let sym = [currencyPair|ADABTC|]
--      let opts = SubmitOrder.optsPostOnly
--      rate <- Bitfinex.marketAveragePrice amt sym
--      Bitfinex.submitOrderMaker env amt sym rate opts
--    itRight "submitCounterOrderMaker" $ \env ->
--      Bitfinex.submitCounterOrderMaker
--        env
--        (OrderId 0)
--        [feeRateMakerBase| 0.001 |]
--        [feeRateMakerQuote| 0.001 |]
--        [profitRate| 0.001 |]
--        SubmitOrder.optsPostOnly
-- focus . itRight "cancelOrderMulti" $ \env ->
--   Bitfinex.cancelOrderMulti
--     env
--     CancelOrderMulti.Everything
-- focus . itRight "dumpIntoQuoteMaker" $ \env ->
--   Bitfinex.dumpIntoQuoteMaker
--     env
--     [currencyPair|XLM:BTC|]
--     SubmitOrder.optsPostOnly
