{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient
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
    submitCounterOrder,
    submitCounterOrderMaker,
    dumpIntoQuote,
    dumpIntoQuoteMaker,
    netWorth,
    candlesLast,
    candlesHist,
    tickers,
    module X,
  )
where

import qualified BitfinexClient.Data.CancelOrderMulti as CancelOrderMulti
import qualified BitfinexClient.Data.Candles as Candles
import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import qualified BitfinexClient.Data.Wallets as Wallets
import BitfinexClient.Import
import BitfinexClient.Import.Internal as X
import BitfinexClient.Indicator.Atr as X
import BitfinexClient.Indicator.Ma as X
import BitfinexClient.Indicator.Mma as X
import BitfinexClient.Indicator.Tr as X
import qualified BitfinexClient.Math as Math
import qualified BitfinexClient.Rpc.Generic as Generic
import qualified Data.Map as Map
import qualified Data.Set as Set

platformStatus ::
  ( MonadIO m
  ) =>
  ExceptT Error m PltStatus
platformStatus =
  Generic.pub @'PlatformStatus [] ()

symbolsDetails ::
  ( MonadIO m
  ) =>
  ExceptT Error m (Map CurrencyPair CurrencyPairConf)
symbolsDetails =
  Generic.pub @'SymbolsDetails [] ()

marketAveragePrice ::
  ( MonadIO m,
    ToRequestParam (Money 'Base act)
  ) =>
  Money 'Base act ->
  CurrencyPair ->
  ExceptT Error m (QuotePerBase act)
marketAveragePrice amt sym =
  Generic.pub
    @'MarketAveragePrice
    [ SomeQueryParam "amount" amt,
      SomeQueryParam "symbol" sym
    ]
    MarketAveragePrice.Request
      { MarketAveragePrice.amount = amt,
        MarketAveragePrice.symbol = sym
      }

feeSummary ::
  ( MonadIO m
  ) =>
  Env ->
  ExceptT Error m FeeSummary.Response
feeSummary env =
  Generic.prv
    @'FeeSummary
    env
    (mempty :: Map Int Int)

wallets ::
  forall crel m.
  ( SingI crel,
    Typeable crel,
    MonadIO m
  ) =>
  Env ->
  ExceptT
    Error
    m
    ( Map
        (CurrencyCode crel)
        ( Map
            Wallets.WalletType
            (Wallets.Response crel)
        )
    )
wallets env =
  Generic.prv
    @'Wallets
    env
    (mempty :: Map Int Int)

spendableExchangeBalance ::
  forall crel m.
  ( SingI crel,
    Typeable crel,
    MonadIO m
  ) =>
  Env ->
  CurrencyCode crel ->
  ExceptT Error m (Money crel 'Sell)
spendableExchangeBalance env cc =
  maybe noMoney Wallets.availableBalance
    . Map.lookup Wallets.Exchange
    . Map.findWithDefault mempty cc
    <$> wallets env
  where
    noMoney =
      case sing :: Sing crel of
        SBase -> [moneyBaseSell|0|]
        SQuote -> [moneyQuoteSell|0|]

retrieveOrders ::
  ( MonadIO m
  ) =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (SomeOrder 'Remote))
retrieveOrders =
  Generic.prv @'RetrieveOrders

ordersHistory ::
  ( MonadIO m
  ) =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (SomeOrder 'Remote))
ordersHistory =
  Generic.prv @'OrdersHistory

getOrders ::
  ( MonadIO m
  ) =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (SomeOrder 'Remote))
getOrders =
  getOrders' 0

getOrders' ::
  ( MonadIO m
  ) =>
  Natural ->
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (SomeOrder 'Remote))
getOrders' attempt env opts = do
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
      getOrders' (attempt + 1) env opts

getOrder ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  ExceptT Error m (SomeOrder 'Remote)
getOrder env id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env (GetOrders.optsIds $ Set.singleton id0)
  except $ maybeToRight (ErrorMissingOrder id0) mOrder

verifyOrder ::
  forall act m.
  ( MonadIO m,
    SingI act
  ) =>
  Env ->
  OrderId ->
  SubmitOrder.Request act ->
  ExceptT Error m (Order act 'Remote)
verifyOrder env id0 req = do
  someRemOrd@(SomeOrder remSing remOrd) <- getOrder env id0
  case testEquality remSing locSing of
    Nothing -> throwE $ ErrorOrderState someRemOrd
    Just Refl -> do
      let locOrd =
            Order
              { orderId =
                  id0,
                orderGroupId =
                  SubmitOrder.groupId opts,
                orderClientId =
                  SubmitOrder.clientId opts
                    <|> orderClientId remOrd,
                orderAmount =
                  SubmitOrder.amount req,
                orderSymbol =
                  SubmitOrder.symbol req,
                orderRate =
                  SubmitOrder.rate req,
                orderStatus =
                  orderStatus remOrd
              }
      if remOrd == locOrd
        then pure remOrd
        else
          throwE $
            ErrorUnverifiedOrder
              (SomeOrder locSing $ coerce locOrd)
              someRemOrd
  where
    opts = SubmitOrder.options req
    locSing = sing :: Sing act

submitOrder ::
  forall act m.
  ( MonadIO m,
    ToRequestParam (Money 'Base act),
    SingI act
  ) =>
  Env ->
  Money 'Base act ->
  CurrencyPair ->
  QuotePerBase act ->
  SubmitOrder.Options act ->
  ExceptT Error m (Order act 'Remote)
submitOrder env amt sym rate opts = do
  let req =
        SubmitOrder.Request
          { SubmitOrder.amount = amt,
            SubmitOrder.symbol = sym,
            SubmitOrder.rate = rate,
            SubmitOrder.options = opts
          }
  order :: Order act 'Remote <-
    Generic.prv @'SubmitOrder env req
  verifyOrder env (orderId order) req

submitOrderMaker ::
  forall act m.
  ( MonadIO m,
    ToRequestParam (Money 'Base act),
    SingI act,
    Typeable act
  ) =>
  Env ->
  Money 'Base act ->
  CurrencyPair ->
  QuotePerBase act ->
  SubmitOrder.Options act ->
  ExceptT Error m (Order act 'Remote)
submitOrderMaker env amt sym rate0 opts0 =
  this 0 rate0
  where
    opts =
      opts0
        { SubmitOrder.flags =
            Set.insert PostOnly $
              SubmitOrder.flags opts0
        }
    this ::
      Int ->
      QuotePerBase act ->
      ExceptT Error m (Order act 'Remote)
    this attempt rate = do
      order <- submitOrder env amt sym rate opts
      if orderStatus order /= PostOnlyCancelled
        then pure order
        else do
          when (attempt >= 10)
            . throwE
            . ErrorOrderState
            $ SomeOrder sing order
          newRate <-
            tryErrorT $ Math.tweakMakerRate rate
          this (attempt + 1) newRate

cancelOrderMulti ::
  ( MonadIO m
  ) =>
  Env ->
  CancelOrderMulti.Request ->
  ExceptT Error m (Map OrderId (SomeOrder 'Remote))
cancelOrderMulti =
  Generic.prv @'CancelOrderMulti

cancelOrderById ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  ExceptT Error m (SomeOrder 'Remote)
cancelOrderById env id0 = do
  mOrder <-
    Map.lookup id0
      <$> cancelOrderMulti
        env
        ( CancelOrderMulti.ByOrderId $ Set.singleton id0
        )
  except $
    maybeToRight (ErrorMissingOrder id0) mOrder

cancelOrderByClientId ::
  ( MonadIO m
  ) =>
  Env ->
  OrderClientId ->
  UTCTime ->
  ExceptT Error m (Maybe (SomeOrder 'Remote))
cancelOrderByClientId env cid utc =
  listToMaybe . elems
    <$> cancelOrderMulti
      env
      ( CancelOrderMulti.ByOrderClientId $
          Set.singleton (cid, utc)
      )

cancelOrderByGroupId ::
  ( MonadIO m
  ) =>
  Env ->
  OrderGroupId ->
  ExceptT Error m (Map OrderId (SomeOrder 'Remote))
cancelOrderByGroupId env gid = do
  cancelOrderMulti env . CancelOrderMulti.ByOrderGroupId $
    Set.singleton gid

submitCounterOrder ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  FeeRate mrel0 'Base ->
  FeeRate mrel1 'Quote ->
  ProfitRate ->
  SubmitOrder.Options 'Sell ->
  ExceptT Error m (Order 'Sell 'Remote)
submitCounterOrder =
  submitCounterOrder' submitOrder

submitCounterOrderMaker ::
  ( MonadIO m
  ) =>
  Env ->
  OrderId ->
  FeeRate 'Maker 'Base ->
  FeeRate 'Maker 'Quote ->
  ProfitRate ->
  SubmitOrder.Options 'Sell ->
  ExceptT Error m (Order 'Sell 'Remote)
submitCounterOrderMaker =
  submitCounterOrder' submitOrderMaker

submitCounterOrder' ::
  ( MonadIO m
  ) =>
  ( Env ->
    Money 'Base 'Sell ->
    CurrencyPair ->
    QuotePerBase 'Sell ->
    SubmitOrder.Options 'Sell ->
    ExceptT Error m (Order 'Sell 'Remote)
  ) ->
  Env ->
  OrderId ->
  FeeRate mrel0 'Base ->
  FeeRate mrel1 'Quote ->
  ProfitRate ->
  SubmitOrder.Options 'Sell ->
  ExceptT Error m (Order 'Sell 'Remote)
submitCounterOrder' submit env id0 feeB feeQ prof opts = do
  someRemOrd@(SomeOrder remSing remOrder) <- getOrder env id0
  case remSing of
    SBuy | orderStatus remOrder == Executed -> do
      (_, exitAmt, exitRate) <-
        except $
          Math.newCounterOrder
            (orderAmount remOrder)
            (orderRate remOrder)
            feeB
            feeQ
            prof
      currentRate <-
        marketAveragePrice exitAmt $
          orderSymbol remOrder
      submit
        env
        exitAmt
        (orderSymbol remOrder)
        (max exitRate currentRate)
        opts
    _ ->
      throwE $
        ErrorOrderState someRemOrd

dumpIntoQuote' ::
  ( MonadIO m
  ) =>
  ( Env ->
    Money 'Base 'Sell ->
    CurrencyPair ->
    QuotePerBase 'Sell ->
    SubmitOrder.Options 'Sell ->
    ExceptT Error m (Order 'Sell 'Remote)
  ) ->
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  ExceptT Error m (Order 'Sell 'Remote)
dumpIntoQuote' submit env sym opts = do
  amt <- spendableExchangeBalance env (currencyPairBase sym)
  rate <- marketAveragePrice amt sym
  catchE
    (submit env amt sym rate opts)
    . const
    $ do
      newAmt <- tryErrorT $ Math.tweakMoneyPip amt
      submit env newAmt sym rate opts

dumpIntoQuote ::
  ( MonadIO m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  ExceptT Error m (Order 'Sell 'Remote)
dumpIntoQuote =
  dumpIntoQuote' submitOrder

dumpIntoQuoteMaker ::
  ( MonadIO m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  ExceptT Error m (Order 'Sell 'Remote)
dumpIntoQuoteMaker =
  dumpIntoQuote' submitOrderMaker

netWorth ::
  ( MonadIO m
  ) =>
  Env ->
  CurrencyCode 'Quote ->
  ExceptT Error m (Money 'Quote 'Sell)
netWorth env ccq = do
  -- Simplify fees (assume it's alwayus Maker and Crypto2Crypto)
  fee <- FeeSummary.makerCrypto2CryptoFee <$> feeSummary env
  syms <- symbolsDetails
  xs0 <- wallets @'Quote env
  res <-
    foldrM
      ( \(ccb, bs1) totalAcc -> do
          let localAcc :: MoneyQuote' =
                foldr
                  ( \amt acc ->
                      unMoney (Wallets.balance amt) |+| acc
                  )
                  (unMoney [moneyQuoteSell|0|])
                  $ Map.elems bs1
          if ccb == ccq
            then
              pure $
                totalAcc |+| localAcc
            else do
              -- In this case we are dealing with Base
              -- money, so we need transform from Quote
              sym <-
                tryErrorT $
                  currencyPairCon (from ccb) ccq
              baseMoney :: Money 'Base 'Sell <-
                tryErrorT . roundMoney $
                  unMoney' @'Quote localAcc
              if baseMoney == [moneyBaseSell|0|]
                then pure totalAcc
                else do
                  price <-
                    marketAveragePrice baseMoney sym
                  pure $
                    totalAcc
                      |+| ( ( unMoney baseMoney
                                |*| unQuotePerBase price
                            )
                              |* (1 - unFeeRate fee)
                          )
      )
      (unMoney [moneyQuoteSell|0|])
      . filter
        ( \(cc, _) ->
            fromRight
              False
              ( flip Map.member syms
                  <$> currencyPairCon (from cc) ccq
              )
              || (cc == ccq)
        )
      $ Map.assocs xs0
  tryErrorT $
    roundMoney' @'Quote res

candlesLast ::
  ( MonadIO m
  ) =>
  CandleTimeFrame ->
  CurrencyPair ->
  Candles.Options ->
  ExceptT Error m Candle
candlesLast tf sym opts =
  Generic.pub
    @'CandlesLast
    ( catMaybes
        [ SomeQueryParam "limit" <$> Candles.limit opts,
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
  ( MonadIO m
  ) =>
  CandleTimeFrame ->
  CurrencyPair ->
  Candles.Options ->
  ExceptT Error m (NonEmpty Candle)
candlesHist tf sym opts =
  Generic.pub
    @'CandlesHist
    ( catMaybes
        [ SomeQueryParam "limit" <$> Candles.limit opts,
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
  ( MonadIO m
  ) =>
  ExceptT Error m (Map CurrencyPair Ticker)
tickers =
  Generic.pub @'Tickers
    [ SomeQueryParam "symbols" ("ALL" :: Text)
    ]
    ()
