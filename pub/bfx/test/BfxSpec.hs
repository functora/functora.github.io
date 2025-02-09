{-# OPTIONS_HADDOCK show-extensions #-}

module BfxSpec
  ( spec,
  )
where

import Bfx
import qualified Bfx.Data.Candles as Candles
import qualified Bfx.Data.GetOrders as GetOrders
import qualified Bfx.Data.MarketAveragePrice as MarketAveragePrice
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.TestEnv
import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude
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
  it "wallets succeeds" $ \env -> do
    res <- tryAny $ Bfx.wallets env
    res `shouldSatisfy` isRight
  it "netWorth succeeds" $ \env -> do
    res <- tryAny . Bfx.netWorth env $ CurrencyCode "BTC"
    res `shouldSatisfy` isRight
  it "candlesLast succeeds" . const $ do
    res <-
      tryAny
        . Bfx.candlesLast Ctf1h adaBtc
        $ Candles.optsDef
        & #ascOrDesc
        .~ Just Desc
    res `shouldSatisfy` isRight
  it "candlesHist succeeds" . const $ do
    res <-
      tryAny
        . Bfx.candlesHist Ctf1h btcUsd
        $ Candles.optsDef
        & #ascOrDesc
        .~ Just Asc
        & #limit
        .~ Just 5
    res `shouldSatisfy` isRight
  it "mkOrder" . const $ do
    let req =
          Bfx.MkOrder
            { Bfx.mkOrderFee = FeeRate 0.001,
              Bfx.mkOrderBuyOrSell = Buy,
              Bfx.mkOrderNetBaseAmt = Nothing,
              Bfx.mkOrderNetQuoteAmt = Nothing,
              Bfx.mkOrderCurrencyPair = adaBtc
            }
    -- let reqQuote = req {Bfx.mkOrderNetQuoteAmt = Just $ MoneyAmount 1}
    -- buyQuote <- Bfx.mkOrder reqQuote
    -- SubmitOrder.baseAmount buyQuote `shouldBe` MoneyAmount 10.01001002
    -- sellQuote <- Bfx.mkOrder reqQuote {Bfx.mkOrderBuyOrSell = Sell}
    -- SubmitOrder.baseAmount sellQuote `shouldBe` MoneyAmount 10
    buyDef <- Bfx.mkOrder req
    SubmitOrder.baseAmount buyDef `shouldBe` MoneyAmount 4.00400401
    sellDef <- Bfx.mkOrder req {Bfx.mkOrderBuyOrSell = Sell}
    SubmitOrder.baseAmount sellDef `shouldBe` MoneyAmount 4
    let reqBase = req {Bfx.mkOrderNetBaseAmt = Just $ MoneyAmount 10}
    buyBase <- Bfx.mkOrder reqBase
    SubmitOrder.baseAmount buyBase `shouldBe` MoneyAmount 10.01001002
    sellBase <- Bfx.mkOrder reqBase {Bfx.mkOrderBuyOrSell = Sell}
    SubmitOrder.baseAmount sellBase `shouldBe` MoneyAmount 10

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
