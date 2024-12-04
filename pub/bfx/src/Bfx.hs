{-# LANGUAGE TypeApplications #-}
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

import qualified Bfx.Data.CancelOrderMulti as CancelOrderMulti
import qualified Bfx.Data.Candles as Candles
import qualified Bfx.Data.FeeSummary as FeeSummary
import qualified Bfx.Data.GetOrders as GetOrders
import qualified Bfx.Data.MarketAveragePrice as MarketAveragePrice
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import qualified Bfx.Data.Wallets as Wallets
import Bfx.Import
import Bfx.Import.Internal as X
import Bfx.Indicator.Atr as X
import Bfx.Indicator.Ma as X
import Bfx.Indicator.Tr as X
import qualified Bfx.Math as Math
import qualified Bfx.Rpc.Generic as Generic
import qualified Data.Map as Map
import qualified Data.Set as Set

platformStatus ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  m PltStatus
platformStatus =
  Generic.pub @'PlatformStatus [] ()

symbolsDetails ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  m (Map CurrencyPair CurrencyPairConf)
symbolsDetails =
  Generic.pub @'SymbolsDetails [] ()

marketAveragePrice ::
  forall (bos :: BuyOrSell) m.
  ( MonadUnliftIO m,
    MonadThrow m,
    SingI bos
  ) =>
  MoneyAmount ->
  CurrencyPair ->
  m QuotePerBase
marketAveragePrice baseAmt sym =
  Generic.pub
    @'MarketAveragePrice
    [ SomeQueryParam "amount" (demote @bos, Base, baseAmt),
      SomeQueryParam "symbol" sym
    ]
    MarketAveragePrice.Request
      { MarketAveragePrice.baseAmount = baseAmt,
        MarketAveragePrice.symbol = sym
      }

feeSummary ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  m FeeSummary.Response
feeSummary env =
  Generic.prv
    @'FeeSummary
    env
    (mempty :: Map Int Int)

wallets ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  m
    ( Map
        CurrencyCode
        (Map Wallets.WalletType Wallets.Response)
    )
wallets env =
  Generic.prv
    @'Wallets
    env
    (mempty :: Map Int Int)

spendableExchangeBalance ::
  ( MonadUnliftIO m,
    MonadThrow m
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
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  GetOrders.Options ->
  m (Map OrderId (SomeOrder 'Remote))
retrieveOrders =
  Generic.prv @'RetrieveOrders

ordersHistory ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  GetOrders.Options ->
  m (Map OrderId (SomeOrder 'Remote))
ordersHistory =
  Generic.prv @'OrdersHistory

getOrders ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  GetOrders.Options ->
  m (Map OrderId (SomeOrder 'Remote))
getOrders =
  getOrders' 0

getOrders' ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Natural ->
  Env ->
  GetOrders.Options ->
  m (Map OrderId (SomeOrder 'Remote))
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
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  OrderId ->
  m (SomeOrder 'Remote)
getOrder env id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env (GetOrders.optsIds $ Set.singleton id0)
  maybe (throw $ ErrorMissingOrder id0) pure mOrder

verifyOrder ::
  forall act m.
  ( MonadUnliftIO m,
    MonadThrow m,
    SingI act
  ) =>
  Env ->
  OrderId ->
  SubmitOrder.Request act ->
  m (Order act 'Remote)
verifyOrder env id0 req = do
  someRemOrd@(SomeOrder remSing remOrd) <- getOrder env id0
  case testEquality remSing locSing of
    Nothing -> throw $ ErrorOrderState someRemOrd
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
                orderBaseAmount =
                  SubmitOrder.baseAmount req,
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
          throw
            $ ErrorUnverifiedOrder
              (SomeOrder locSing $ coerce locOrd)
              someRemOrd
  where
    opts = SubmitOrder.options req
    locSing = sing :: Sing act

submitOrder ::
  forall (bos :: BuyOrSell) m.
  ( MonadUnliftIO m,
    MonadThrow m,
    -- ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount |+| bos)),
    -- ToRequestParam (Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos)),
    Typeable bos,
    SingI bos
  ) =>
  Env ->
  MoneyAmount ->
  CurrencyPair ->
  QuotePerBase ->
  SubmitOrder.Options bos ->
  m (Order bos 'Remote)
submitOrder env baseAmt sym rate opts = do
  let req =
        SubmitOrder.Request
          { SubmitOrder.baseAmount = baseAmt,
            SubmitOrder.symbol = sym,
            SubmitOrder.rate = rate,
            SubmitOrder.options = opts
          }
  order :: Order bos 'Remote <- Generic.prv @'SubmitOrder env req
  verifyOrder env (orderId order) req

submitOrderMaker ::
  forall (bos :: BuyOrSell) m.
  ( MonadUnliftIO m,
    MonadThrow m,
    Typeable bos,
    SingI bos
    -- ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount |+| bos)),
    -- ToRequestParam (Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos)),
    -- MoneyTags (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount |+| bos),
    -- MoneyTags (Tags 'Unsigned |+| 'QuotePerBase |+| bos),
    -- HasTag bos (Tags 'Unsigned |+| 'QuotePerBase |+| bos)
  ) =>
  Env ->
  MoneyAmount ->
  CurrencyPair ->
  QuotePerBase ->
  SubmitOrder.Options bos ->
  m (Order bos 'Remote)
submitOrderMaker env baseAmt sym rate0 opts0 =
  submitOrderMakerRec @bos env baseAmt sym 0 rate0 opts
  where
    opts =
      opts0
        { SubmitOrder.flags =
            Set.insert PostOnly $ SubmitOrder.flags opts0
        }

submitOrderMakerRec ::
  forall (bos :: BuyOrSell) m.
  ( MonadUnliftIO m,
    MonadThrow m,
    Typeable bos,
    SingI bos
    -- ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount |+| bos)),
    -- ToRequestParam (Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos)),
    -- MoneyTags (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount |+| bos),
    -- MoneyTags (Tags 'Unsigned |+| 'QuotePerBase |+| bos),
    -- HasTag bos (Tags 'Unsigned |+| 'QuotePerBase |+| bos)
  ) =>
  Env ->
  MoneyAmount ->
  CurrencyPair ->
  Int ->
  QuotePerBase ->
  SubmitOrder.Options bos ->
  m (Order bos 'Remote)
submitOrderMakerRec env baseAmt sym attempt rate opts = do
  order :: Order bos 'Remote <- submitOrder @bos env baseAmt sym rate opts
  if orderStatus order /= PostOnlyCancelled
    then pure order
    else do
      when (attempt >= 10)
        . throw
        . ErrorOrderState
        $ SomeOrder (sing :: Sing bos) order
      newRate <- Math.tweakMakerRate (demote @bos) rate
      submitOrderMakerRec env baseAmt sym (attempt + 1) newRate opts

cancelOrderMulti ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  CancelOrderMulti.Request ->
  m (Map OrderId (SomeOrder 'Remote))
cancelOrderMulti =
  Generic.prv @'CancelOrderMulti

cancelOrderById ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  OrderId ->
  m (SomeOrder 'Remote)
cancelOrderById env id0 = do
  mOrder <-
    Map.lookup id0
      <$> cancelOrderMulti
        env
        ( CancelOrderMulti.ByOrderId $ Set.singleton id0
        )
  maybe (throw $ ErrorMissingOrder id0) pure mOrder

cancelOrderByClientId ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  OrderClientId ->
  UTCTime ->
  m (Maybe (SomeOrder 'Remote))
cancelOrderByClientId env cid utc =
  listToMaybe
    . elems
    <$> cancelOrderMulti
      env
      ( CancelOrderMulti.ByOrderClientId
          $ Set.singleton (cid, utc)
      )

cancelOrderByGroupId ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  OrderGroupId ->
  m (Map OrderId (SomeOrder 'Remote))
cancelOrderByGroupId env gid = do
  cancelOrderMulti env
    . CancelOrderMulti.ByOrderGroupId
    $ Set.singleton gid

submitCounterOrder ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  OrderId ->
  (MoneyAmount -> QuotePerBase -> Math.CounterArgs) ->
  -- Money (Tags 'Unsigned |+| 'FeeRate |+| 'Base) ->
  -- Money (Tags 'Unsigned |+| 'FeeRate |+| 'Quote) ->
  -- Money (Tags 'Unsigned |+| 'ProfitRate) ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
submitCounterOrder =
  submitCounterOrder' submitOrder

submitCounterOrderMaker ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  OrderId ->
  (MoneyAmount -> QuotePerBase -> Math.CounterArgs) ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
submitCounterOrderMaker =
  submitCounterOrder' submitOrderMaker

submitCounterOrder' ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  ( Env ->
    MoneyAmount ->
    CurrencyPair ->
    QuotePerBase ->
    SubmitOrder.Options 'Sell ->
    m (Order 'Sell 'Remote)
  ) ->
  Env ->
  OrderId ->
  (MoneyAmount -> QuotePerBase -> Math.CounterArgs) ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
submitCounterOrder' submit env id0 mkCounter opts = do
  someRemOrd@(SomeOrder remSing remOrder) <- getOrder env id0
  let sym = orderSymbol remOrder
  case remSing of
    SBuy | orderStatus remOrder == Executed -> do
      counter <-
        Math.newCounterOrder
          . mkCounter (orderBaseAmount remOrder)
          $ orderRate remOrder
      let exitAmt = Math.counterExitNetBaseLoss counter
      let exitRate = Math.counterExitQuotePerBase counter
      currentRate <- marketAveragePrice @'Sell exitAmt sym
      submit
        env
        exitAmt
        sym
        (max exitRate currentRate)
        opts
    _ ->
      throw
        $ ErrorOrderState someRemOrd

dumpIntoQuote' ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  ( Env ->
    MoneyAmount ->
    CurrencyPair ->
    QuotePerBase ->
    SubmitOrder.Options 'Sell ->
    m (Order 'Sell 'Remote)
  ) ->
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
dumpIntoQuote' submit env sym opts = do
  amt <- spendableExchangeBalance env (currencyPairBase sym)
  rate <- marketAveragePrice @'Sell amt sym
  catchAny
    (submit env amt sym rate opts)
    . const
    $ do
      newAmt <- Math.tweakMoneyPip Sell amt
      submit env newAmt sym rate opts

dumpIntoQuote ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
dumpIntoQuote =
  dumpIntoQuote' submitOrder

dumpIntoQuoteMaker ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
dumpIntoQuoteMaker =
  dumpIntoQuote' submitOrderMaker

netWorth ::
  ( MonadUnliftIO m,
    MonadThrow m
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
                    marketAveragePrice @'Sell baseMoney sym
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
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  CandleTimeFrame ->
  CurrencyPair ->
  Candles.Options ->
  m Candle
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
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  CandleTimeFrame ->
  CurrencyPair ->
  Candles.Options ->
  m (NonEmpty Candle)
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
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  m (Map CurrencyPair Ticker)
tickers =
  Generic.pub @'Tickers
    [ SomeQueryParam "symbols" ("ALL" :: Text)
    ]
    ()
