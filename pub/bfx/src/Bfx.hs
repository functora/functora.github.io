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
  forall (act :: BuyOrSell) m.
  ( MonadUnliftIO m,
    MonadThrow m,
    ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| act)),
    Typeable act
  ) =>
  Money (Tags 'Unsigned |+| 'Base |+| act) ->
  CurrencyPair ->
  m (Money (Tags 'Unsigned |+| 'QuotePerBase |+| act))
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
  m (Money (Tags 'Unsigned))
spendableExchangeBalance env cc =
  maybe (newMoney 0) Wallets.availableBalance
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
    ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| bos)),
    ToRequestParam (Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos)),
    SingI bos
  ) =>
  Env ->
  Money (Tags 'Unsigned |+| 'Base |+| bos) ->
  CurrencyPair ->
  Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos) ->
  SubmitOrder.Options bos ->
  m (Order bos 'Remote)
submitOrder env amt sym rate opts = do
  let req =
        SubmitOrder.Request
          { SubmitOrder.amount = amt,
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
    ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| bos)),
    ToRequestParam (Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos)),
    MoneyTags (Tags 'Unsigned |+| 'Base |+| bos),
    MoneyTags (Tags 'Unsigned |+| 'QuotePerBase |+| bos),
    GetTag bos (Tags 'Unsigned |+| 'QuotePerBase |+| bos),
    Typeable bos
  ) =>
  Env ->
  Money (Tags 'Unsigned |+| 'Base |+| bos) ->
  CurrencyPair ->
  Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos) ->
  SubmitOrder.Options bos ->
  m (Order bos 'Remote)
submitOrderMaker env amt sym rate0 opts0 =
  submitOrderMakerRec @bos env amt sym 0 rate0 opts
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
    ToRequestParam (Money (Tags 'Unsigned |+| 'Base |+| bos)),
    ToRequestParam (Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos)),
    MoneyTags (Tags 'Unsigned |+| 'Base |+| bos),
    MoneyTags (Tags 'Unsigned |+| 'QuotePerBase |+| bos),
    GetTag bos (Tags 'Unsigned |+| 'QuotePerBase |+| bos),
    Typeable bos
  ) =>
  Env ->
  Money (Tags 'Unsigned |+| 'Base |+| bos) ->
  CurrencyPair ->
  Int ->
  Money (Tags 'Unsigned |+| 'QuotePerBase |+| bos) ->
  SubmitOrder.Options bos ->
  m (Order bos 'Remote)
submitOrderMakerRec env amt sym attempt rate opts = do
  order :: Order bos 'Remote <- submitOrder @bos env amt sym rate opts
  if orderStatus order /= PostOnlyCancelled
    then pure order
    else do
      when (attempt >= 10)
        . throw
        . ErrorOrderState
        $ SomeOrder (sing :: Sing bos) order
      newRate <- Math.tweakMakerRate rate
      submitOrderMakerRec env amt sym (attempt + 1) newRate opts

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
  Money (Tags 'Unsigned |+| 'FeeRate |+| 'Base) ->
  Money (Tags 'Unsigned |+| 'FeeRate |+| 'Quote) ->
  Money (Tags 'Unsigned |+| 'ProfitRate) ->
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
  Money (Tags 'Unsigned |+| 'FeeRate |+| 'Base) ->
  Money (Tags 'Unsigned |+| 'FeeRate |+| 'Quote) ->
  Money (Tags 'Unsigned |+| 'ProfitRate) ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
submitCounterOrderMaker =
  submitCounterOrder' submitOrderMaker

submitCounterOrder' ::
  ( MonadUnliftIO m,
    MonadThrow m
  ) =>
  ( Env ->
    Money (Tags 'Unsigned |+| 'Base |+| 'Sell) ->
    CurrencyPair ->
    Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell) ->
    SubmitOrder.Options 'Sell ->
    m (Order 'Sell 'Remote)
  ) ->
  Env ->
  OrderId ->
  Money (Tags 'Unsigned |+| 'FeeRate |+| 'Base) ->
  Money (Tags 'Unsigned |+| 'FeeRate |+| 'Quote) ->
  Money (Tags 'Unsigned |+| 'ProfitRate) ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
submitCounterOrder' submit env id0 feeB feeQ prof opts = do
  someRemOrd@(SomeOrder remSing remOrder) <- getOrder env id0
  case remSing of
    SBuy | orderStatus remOrder == Executed -> do
      (_, exitAmt, exitRate) <-
        Math.newCounterOrder
          (tagMoney @'Gross (orderAmount remOrder))
          (tagMoney @'Net (orderRate remOrder))
          feeB
          feeQ
          (tagMoney @'Quote . tagMoney @'Buy $ tagMoney @'Net prof)
      currentRate <-
        marketAveragePrice (unTagMoney @'Net exitAmt)
          $ orderSymbol remOrder
      submit
        env
        (unTagMoney @'Net exitAmt)
        (orderSymbol remOrder)
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
    Money (Tags 'Unsigned |+| 'Base |+| 'Sell) ->
    CurrencyPair ->
    Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell) ->
    SubmitOrder.Options 'Sell ->
    m (Order 'Sell 'Remote)
  ) ->
  Env ->
  CurrencyPair ->
  SubmitOrder.Options 'Sell ->
  m (Order 'Sell 'Remote)
dumpIntoQuote' submit env sym opts = do
  amt <- spendableExchangeBalance env (currencyPairBase sym)
  rate <- marketAveragePrice (tagMoney @'Sell $ tagMoney @'Base amt) sym
  catchAny
    (submit env (tagMoney @'Sell $ tagMoney @'Base amt) sym rate opts)
    . const
    $ do
      newAmt <- Math.tweakMoneyPip (tagMoney @'Sell $ tagMoney @'Base amt)
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
  m (Money (Tags 'Unsigned))
netWorth env ccq = do
  -- Simplify fees (assume it's alwayus Maker and Crypto2Crypto)
  fee <- FeeSummary.makerCrypto2CryptoFee <$> feeSummary env
  syms <- symbolsDetails
  xs0 <- wallets env
  res <-
    foldrM
      ( \(ccb, bs1) totalAcc -> do
          let localAcc :: Money (Tags 'Unsigned) =
                foldr
                  ( \amt acc ->
                      Wallets.balance amt `addMoney` acc
                  )
                  (newMoney 0)
                  $ Map.elems bs1
          if ccb == ccq
            then pure $ totalAcc `addMoney` localAcc
            else do
              -- In this case we are dealing with Base
              -- money, so we need transform from Quote
              sym <- currencyPairCon (from ccb) $ Tagged @'Quote ccq
              baseMoney :: Money (Tags 'Unsigned |+| 'Base |+| 'Sell) <-
                fmap (tagMoney @'Base . tagMoney @'Sell) $ roundMoney localAcc
              if baseMoney == newMoney 0
                then pure totalAcc
                else do
                  price :: Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell) <-
                    marketAveragePrice baseMoney sym
                  pure
                    . addMoney totalAcc
                    . unTagMoney @'Net
                    . unTagMoney @'Sell
                    . unTagMoney @'Quote
                    $ deductFee
                      fee
                      ( tagMoney @'Gross
                          $ exchangeMoney @(Tags 'Unsigned |+| 'Sell)
                            price
                            baseMoney
                      )
      )
      (newMoney 0)
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
  roundMoney res

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
