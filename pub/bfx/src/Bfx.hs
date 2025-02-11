{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx
  ( platformStatus,
    symbolsDetails,
    marketAveragePrice,
    feeSummary,
    wallets,
    spendableExchangeBalance,
    retrieveOrders,
    ordersHistory,
    getOrders,
    getOrder,
    verifyOrder,
    submitOrder,
    submitOrderMaker,
    cancelOrderMulti,
    cancelOrderById,
    cancelOrderByClientId,
    cancelOrderByGroupId,
    dumpIntoQuote,
    dumpIntoQuoteMaker,
    netWorth,
    candlesLast,
    candlesHist,
    tickers,
    MkOrder (..),
    mkOrder,
    MkCounterOrder (..),
    mkCounterOrder,
    module X,
  )
where

import Bfx.Class.FromRpc as X
import Bfx.Class.ToBaseUrl as X
import Bfx.Class.ToPathPieces as X
import Bfx.Class.ToRequestMethod as X
import Bfx.Class.ToRequestParam as X
import qualified Bfx.Data.CancelOrderMulti as CancelOrderMulti
import qualified Bfx.Data.Candles as Candles
import Bfx.Data.Env as X
import qualified Bfx.Data.FeeSummary as FeeSummary
import qualified Bfx.Data.GetOrders as GetOrders
import Bfx.Data.Kind as X
import qualified Bfx.Data.MarketAveragePrice as MarketAveragePrice
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.Data.Type as X
import qualified Bfx.Data.Wallets as Wallets
import Bfx.Data.Web as X
import Bfx.Indicator.Atr as X
import Bfx.Indicator.Ma as X
import Bfx.Indicator.Tr as X
import Bfx.Math as X
import Bfx.Parser as X
import qualified Bfx.Rpc.Generic as Generic
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Functora.Money
import Functora.Prelude

platformStatus ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m PltStatus
platformStatus =
  Generic.pub @'PlatformStatus mempty emptyReq

symbolsDetails ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m (Map CurrencyPair CurrencyPairConf)
symbolsDetails =
  Generic.pub @'SymbolsDetails mempty emptyReq

marketAveragePrice ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  MarketAveragePrice.Request ->
  m QuotePerBase
marketAveragePrice args =
  Generic.pub
    @'MarketAveragePrice
    [ SomeQueryParam "amount" amt,
      SomeQueryParam "symbol" sym
    ]
    args
  where
    amt =
      ( MarketAveragePrice.buyOrSell args,
        MarketAveragePrice.baseAmount args
      )
    sym =
      MarketAveragePrice.symbol args

feeSummary ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  m FeeSummary.Response
feeSummary env =
  Generic.prv @'FeeSummary env emptyReq

wallets ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  m
    ( Map
        CurrencyCode
        (Map Wallets.WalletType Wallets.Response)
    )
wallets env =
  Generic.prv @'Wallets env emptyReq

spendableExchangeBalance ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  CurrencyCode ->
  m MoneyAmount
spendableExchangeBalance env cc =
  maybe (MoneyAmount 0) Wallets.availableBalance
    . Map.lookup Wallets.Exchange
    . Map.findWithDefault mempty cc
    <$> wallets env

retrieveOrders ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  GetOrders.Options ->
  m (Map OrderId Order)
retrieveOrders =
  Generic.prv @'RetrieveOrders

ordersHistory ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  GetOrders.Options ->
  m (Map OrderId Order)
ordersHistory =
  Generic.prv @'OrdersHistory

getOrders ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  GetOrders.Options ->
  m (Map OrderId Order)
getOrders =
  getOrdersRec 0

getOrdersRec ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Natural ->
  Env ->
  GetOrders.Options ->
  m (Map OrderId Order)
getOrdersRec attempt env opts = do
  xs0 <- retrieveOrders env opts
  xs1 <- ordersHistory env opts
  let xs = xs1 <> xs0
  if ( all (`Map.member` xs)
        . toList
        $ GetOrders.orderIds opts
     )
    || (attempt > 7)
    then pure xs
    else do
      liftIO $ threadDelay 250000
      getOrdersRec (attempt + 1) env opts

getOrder ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  OrderId ->
  m Order
getOrder env id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env (GetOrders.optsIds $ Set.singleton id0)
  maybe (throw $ ErrorMissingOrder id0) pure mOrder

verifyOrder ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  OrderId ->
  SubmitOrder.Request ->
  m Order
verifyOrder env id0 req = do
  remOrd <- getOrder env id0
  let locOrd =
        Order
          { orderId = id0,
            orderGroupId =
              SubmitOrder.groupId opts,
            orderClientId =
              SubmitOrder.clientId opts
                <|> orderClientId remOrd,
            orderBaseAmount =
              SubmitOrder.baseAmount req,
            orderSymbol =
              SubmitOrder.symbol req,
            orderRate =
              SubmitOrder.rate req,
            orderStatus =
              orderStatus remOrd,
            orderBuyOrSell =
              SubmitOrder.buyOrSell req,
            orderLocalOrRemote =
              Local
          }
  if remOrd == locOrd {orderLocalOrRemote = Remote}
    then pure remOrd
    else
      throw
        $ ErrorUnverifiedOrder
          (Tagged @'Local locOrd)
          (Tagged @'Remote remOrd)
  where
    opts = SubmitOrder.options req

submitOrder ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  SubmitOrder.Request ->
  m Order
submitOrder env req = do
  order <- Generic.prv @'SubmitOrder env req
  verifyOrder env (orderId order) req

submitOrderMaker ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  SubmitOrder.Request ->
  m Order
submitOrderMaker env req = do
  next <- tweakQuotePerBase (req ^. #buyOrSell) (req ^. #rate)
  submitOrderMakerRec 0 env
    $ req
    & #rate
    .~ next
    & #options
    . #flags
    %~ Set.insert PostOnly

submitOrderMakerRec ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Int ->
  Env ->
  SubmitOrder.Request ->
  m Order
submitOrderMakerRec attempt env req = do
  order <- submitOrder env req
  if orderStatus order /= PostOnlyCancelled
    then pure order
    else do
      when (attempt >= 10) . throw $ ErrorRemoteOrderState order
      next <- tweakQuotePerBase (req ^. #buyOrSell) (req ^. #rate)
      submitOrderMakerRec (attempt + 1) env $ req & #rate .~ next

cancelOrderMulti ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  CancelOrderMulti.Request ->
  m (Map OrderId Order)
cancelOrderMulti =
  Generic.prv @'CancelOrderMulti

cancelOrderById ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  OrderId ->
  m Order
cancelOrderById env id0 = do
  mOrder <-
    Map.lookup id0
      <$> cancelOrderMulti
        env
        ( CancelOrderMulti.ByOrderId $ Set.singleton id0
        )
  maybe (throw $ ErrorMissingOrder id0) pure mOrder

cancelOrderByClientId ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  OrderClientId ->
  UTCTime ->
  m (Maybe Order)
cancelOrderByClientId env cid utc =
  listToMaybe
    . elems
    <$> cancelOrderMulti
      env
      ( CancelOrderMulti.ByOrderClientId
          $ Set.singleton (cid, utc)
      )

cancelOrderByGroupId ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  OrderGroupId ->
  m (Map OrderId Order)
cancelOrderByGroupId env gid = do
  cancelOrderMulti env
    . CancelOrderMulti.ByOrderGroupId
    $ Set.singleton gid

mkDumpIntoQuote ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ( Env -> SubmitOrder.Request -> m Order
  ) ->
  Env ->
  CurrencyPair ->
  SubmitOrder.Options ->
  m Order
mkDumpIntoQuote submit env sym opts = do
  amt <- spendableExchangeBalance env (currencyPairBase sym)
  rate <-
    marketAveragePrice
      MarketAveragePrice.Request
        { MarketAveragePrice.buyOrSell = Sell,
          MarketAveragePrice.baseAmount = amt,
          MarketAveragePrice.symbol = sym
        }
  let mkSubmit baseAmount =
        submit
          env
          SubmitOrder.Request
            { SubmitOrder.buyOrSell = Sell,
              SubmitOrder.baseAmount = baseAmount,
              SubmitOrder.symbol = sym,
              SubmitOrder.rate = rate,
              SubmitOrder.options = opts
            }
  catchAny (mkSubmit amt)
    . const
    $ tweakMoneyAmount Sell amt
    >>= mkSubmit

dumpIntoQuote ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options ->
  m Order
dumpIntoQuote =
  mkDumpIntoQuote submitOrder

dumpIntoQuoteMaker ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options ->
  m Order
dumpIntoQuoteMaker =
  mkDumpIntoQuote submitOrderMaker

netWorth ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Env ->
  CurrencyCode ->
  m MoneyAmount
netWorth env ccq = do
  -- Simplify fees (assume it's alwayus Maker and Crypto2Crypto)
  fee <- FeeSummary.makerCrypto2CryptoFee <$> feeSummary env
  syms <- symbolsDetails
  xs0 <- wallets env
  res <-
    foldrM
      ( \(ccb, bs1) totalAcc -> do
          let localAcc =
                foldr
                  ( \amt acc ->
                      unMoneyAmount (Wallets.balance amt) + acc
                  )
                  0
                  $ Map.elems bs1
          if ccb == ccq
            then pure $ totalAcc + localAcc
            else do
              -- In this case we are dealing with Base
              -- money, so we need transform from Quote
              sym <- currencyPairCon (from ccb) $ Tagged @'Quote ccq
              baseMoney <- roundMoneyAmount $ MoneyAmount localAcc
              if baseMoney == MoneyAmount 0
                then pure totalAcc
                else do
                  price <-
                    marketAveragePrice
                      MarketAveragePrice.Request
                        { MarketAveragePrice.buyOrSell = Sell,
                          MarketAveragePrice.baseAmount = baseMoney,
                          MarketAveragePrice.symbol = sym
                        }
                  pure
                    $ ( totalAcc
                          + ( unMoneyAmount baseMoney
                                * unQuotePerBase price
                                * (1 - unFeeRate fee)
                            )
                      )
      )
      0
      . filter
        ( \(cc, _) ->
            fromRight
              False
              ( flip Map.member syms
                  <$> currencyPairCon (from cc) (Tagged @'Quote ccq)
              )
              || (cc == ccq)
        )
      $ Map.assocs xs0
  roundMoneyAmount $ MoneyAmount res

candlesLast ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CandleTimeFrame ->
  CurrencyPair ->
  Candles.Options ->
  m Candle
candlesLast tf sym opts =
  Generic.pub
    @'CandlesLast
    ( catMaybes
        [ SomeQueryParam "sort" <$> Candles.ascOrDesc opts,
          SomeQueryParam "limit" <$> Candles.limit opts,
          SomeQueryParam "start" <$> Candles.start opts,
          SomeQueryParam "end" <$> Candles.end opts
        ]
    )
    Candles.Request
      { Candles.timeFrame = tf,
        Candles.symbol = sym,
        Candles.section = Candles.Last,
        Candles.options = opts
      }

candlesHist ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CandleTimeFrame ->
  CurrencyPair ->
  Candles.Options ->
  m (NonEmpty Candle)
candlesHist tf sym opts =
  Generic.pub
    @'CandlesHist
    ( catMaybes
        [ SomeQueryParam "sort" <$> Candles.ascOrDesc opts,
          SomeQueryParam "limit" <$> Candles.limit opts,
          SomeQueryParam "start" <$> Candles.start opts,
          SomeQueryParam "end" <$> Candles.end opts
        ]
    )
    Candles.Request
      { Candles.timeFrame = tf,
        Candles.symbol = sym,
        Candles.section = Candles.Hist,
        Candles.options = opts
      }

tickers ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m (Map CurrencyPair Ticker)
tickers =
  Generic.pub @'Tickers
    [ SomeQueryParam "symbols" ("ALL" :: Text)
    ]
    emptyReq

type role MkOrder representational

data MkOrder m = MkOrder
  { mkOrderFee :: FeeRate,
    mkOrderBuyOrSell :: BuyOrSell,
    --
    -- NOTE : base amt has higher priority!
    --
    mkOrderNetBaseAmt :: Maybe MoneyAmount,
    mkOrderNetQuoteAmt :: Maybe MoneyAmount,
    mkOrderCurrencyPair :: CurrencyPair,
    mkOrderSymbolsDetails :: m (Map CurrencyPair CurrencyPairConf),
    mkOrderMarketAveragePrice :: MarketAveragePrice.Request -> m QuotePerBase
  }
  deriving stock (Generic)

mkOrder :: (MonadThrow m) => MkOrder m -> m SubmitOrder.Request
mkOrder args = do
  netBaseAmt <-
    maybe
      ( do
          syms <- mkOrderSymbolsDetails args
          minBase <-
            maybe
              (throwString $ inspect @Text sym <> " is missing!")
              (pure . currencyPairMinOrderBaseAmt)
              $ Map.lookup sym syms
          maybe
            (pure minBase)
            ( \netQuoteAmt -> do
                price <-
                  mkOrderMarketAveragePrice
                    args
                    MarketAveragePrice.Request
                      { MarketAveragePrice.buyOrSell = bos,
                        MarketAveragePrice.baseAmount = minBase,
                        MarketAveragePrice.symbol = sym
                      }
                pure
                  . max minBase
                  . MoneyAmount
                  $ unMoneyAmount netQuoteAmt
                  / unQuotePerBase price
            )
            $ mkOrderNetQuoteAmt args
      )
      pure
      $ mkOrderNetBaseAmt args
  grossBaseAmt <-
    case bos of
      Buy ->
        tweakMoneyAmount Buy
          . MoneyAmount
          $ unMoneyAmount netBaseAmt
          / (1 - unFeeRate (mkOrderFee args))
      Sell ->
        pure netBaseAmt
  price <-
    mkOrderMarketAveragePrice
      args
      MarketAveragePrice.Request
        { MarketAveragePrice.buyOrSell = bos,
          MarketAveragePrice.baseAmount = grossBaseAmt,
          MarketAveragePrice.symbol = sym
        }
  pure
    SubmitOrder.Request
      { SubmitOrder.buyOrSell = bos,
        SubmitOrder.baseAmount = grossBaseAmt,
        SubmitOrder.symbol = sym,
        SubmitOrder.rate = price,
        SubmitOrder.options = SubmitOrder.optsDef
      }
  where
    bos = mkOrderBuyOrSell args
    sym = mkOrderCurrencyPair args

type role MkCounterOrder representational

data MkCounterOrder m = MkCounterOrder
  { mkCounterOrderEnterBuyOrSell :: BuyOrSell,
    mkCounterOrderEnterGrossBase :: MoneyAmount,
    mkCounterOrderEnterQuotePerBase :: QuotePerBase,
    mkCounterOrderCurrencyPair :: CurrencyPair,
    mkCounterOrderEnterFee :: FeeRate,
    mkCounterOrderExitFee :: FeeRate,
    mkCounterOrderProfit :: ProfitRate,
    mkCounterOrderMarketAveragePrice ::
      MarketAveragePrice.Request ->
      m QuotePerBase
  }
  deriving stock (Generic)

mkCounterOrder :: (MonadThrow m) => MkCounterOrder m -> m SubmitOrder.Request
mkCounterOrder args = do
  exitBase <-
    tweakMoneyAmount exitBos exitGrossBase
  exitRate <-
    roundQuotePerBase
      . QuotePerBase
      $ unMoneyAmount exitGrossQuote
      / unMoneyAmount exitBase
  currentRate <-
    mkCounterOrderMarketAveragePrice
      args
      MarketAveragePrice.Request
        { MarketAveragePrice.buyOrSell = exitBos,
          MarketAveragePrice.baseAmount = exitBase,
          MarketAveragePrice.symbol = sym
        }
  let bestRate =
        case exitBos of
          Buy -> min exitRate currentRate
          Sell -> max exitRate currentRate
  pure
    SubmitOrder.Request
      { SubmitOrder.buyOrSell = exitBos,
        SubmitOrder.baseAmount = exitBase,
        SubmitOrder.symbol = sym,
        SubmitOrder.rate = bestRate,
        SubmitOrder.options = SubmitOrder.optsDef
      }
  where
    sym :: CurrencyPair
    sym = mkCounterOrderCurrencyPair args
    enterBos :: BuyOrSell
    enterBos = mkCounterOrderEnterBuyOrSell args
    exitBos :: BuyOrSell
    exitBos = nextEnum enterBos
    enterRate :: QuotePerBase
    enterRate = mkCounterOrderEnterQuotePerBase args
    enterBase :: MoneyAmount
    enterBase = mkCounterOrderEnterGrossBase args
    enterNetLoss :: MoneyAmount
    enterNetLoss =
      MoneyAmount $ case enterBos of
        Buy ->
          -- Quote
          unMoneyAmount enterBase
            * unQuotePerBase enterRate
        Sell ->
          -- Base
          unMoneyAmount enterBase
    enterNetGain :: MoneyAmount
    enterNetGain =
      MoneyAmount $ case enterBos of
        Buy ->
          -- Base
          unMoneyAmount enterBase
            * (1 - unFeeRate enterFeeRate)
        Sell ->
          -- Quote
          unMoneyAmount enterBase
            * unQuotePerBase enterRate
            * (1 - unFeeRate enterFeeRate)
    exitGrossGain :: MoneyAmount
    exitGrossGain =
      --
      -- Buy = Quote
      -- Sell = Base
      --
      MoneyAmount
        $ unMoneyAmount enterNetLoss
        * (1 + unProfitRate profitRate)
        / (1 - unFeeRate exitFeeRate)
    exitGrossBase :: MoneyAmount
    exitGrossBase =
      case enterBos of
        Buy -> enterNetGain
        Sell -> exitGrossGain
    exitGrossQuote :: MoneyAmount
    exitGrossQuote =
      case enterBos of
        Buy -> exitGrossGain
        Sell -> enterNetGain
    enterFeeRate :: FeeRate
    enterFeeRate = mkCounterOrderEnterFee args
    exitFeeRate :: FeeRate
    exitFeeRate = mkCounterOrderExitFee args
    profitRate :: ProfitRate
    profitRate = mkCounterOrderProfit args
