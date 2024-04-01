{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Class.FromRpc
  ( FromRpc (..),
  )
where

import qualified Bfx.Data.FeeSummary as FeeSummary
import Bfx.Data.Kind
import Bfx.Data.Metro
import Bfx.Data.Type
import qualified Bfx.Data.Wallets as Wallets
import Bfx.Data.Web
import Bfx.Import.External
import Bfx.Parser
import Data.Aeson.Lens
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Lens.Micro hiding (at)

class FromRpc (method :: Method) res where
  fromRpc :: RawResponse -> Either Text res

instance FromRpc 'PlatformStatus PltStatus where
  fromRpc (RawResponse raw) = do
    ss <-
      maybeToRight
        "PltStatus is missing"
        $ raw
        ^? nth 0 . _Integral
    case ss :: Natural of
      1 -> Right PltOperative
      0 -> Right PltMaintenance
      _ -> Left "Incorrect PltStatus"

instance FromRpc 'CancelOrderMulti (Map OrderId (SomeOrder 'Remote)) where
  fromRpc (RawResponse raw) = do
    xs <-
      maybeToRight
        "Order Array is missing"
        $ raw
        ^? nth 4
    parseOrderMap xs

instance
  FromRpc
    'RetrieveOrders
    (Map OrderId (SomeOrder 'Remote))
  where
  fromRpc (RawResponse raw) =
    parseOrderMap raw

instance
  FromRpc
    'OrdersHistory
    (Map OrderId (SomeOrder 'Remote))
  where
  fromRpc (RawResponse raw) =
    parseOrderMap raw

instance (SingI act) => FromRpc 'SubmitOrder (Order act 'Remote) where
  fromRpc (RawResponse raw) = do
    rawOrder <-
      maybeToRight
        "Order is missing"
        $ raw
        ^? nth 4 . nth 0
    SomeOrder orderSing order <- parseOrder rawOrder
    case testEquality (sing :: Sing act) orderSing of
      Nothing -> Left "Incorrect ExchangeAction"
      Just Refl -> pure order

instance (RateTags tags) => FromRpc 'MarketAveragePrice (Money tags) where
  fromRpc (RawResponse raw) = do
    x <-
      maybeToRight
        "QuotePerBase is missing"
        (toRational <$> raw ^? nth 0 . _Number)
    first (const $ "QuotePerBase is invalid " <> inspect x)
      . roundQuotePerBase
      . newMoney
      $ unsafeFrom @Rational @(Ratio Natural) x

instance FromRpc 'FeeSummary FeeSummary.Response where
  fromRpc (RawResponse raw) = do
    x0 <- parse 0 0 (money @'Maker) "makerCrypto2CryptoFee"
    x1 <- parse 0 1 (money @'Maker) "makerCrypto2StableFee"
    x2 <- parse 0 2 (money @'Maker) "makerCrypto2FiatFee"
    x3 <- parse 0 5 (pure . RebateRate) "makerDerivativeRebate"
    x4 <- parse 1 0 (money @'Taker) "takerCrypto2CryptoFee"
    x5 <- parse 1 1 (money @'Taker) "takerCrypto2StableFee"
    x6 <- parse 1 2 (money @'Taker) "takerCrypto2FiatFee"
    x7 <- parse 1 5 (money @'Taker) "takerDerivativeFee"
    pure
      $ FeeSummary.Response x0 x1 x2 x3 x4 x5 x6 x7
    where
      money ::
        forall tag.
        ( CashTags (Tags 'Unsigned |+| 'FeeRate |+| tag)
        ) =>
        Rational ->
        Either Text (Money (Tags 'Unsigned |+| 'FeeRate |+| tag))
      money =
        bimap inspect newMoney
          . tryFrom @Rational @(Ratio Natural)
      parse ::
        Int ->
        Int ->
        (Rational -> Either Text c) ->
        Text ->
        Either Text c
      parse ix0 ix1 con label =
        ( first (const $ label <> " is invalid")
            . con
            . toRational
        )
          <=< maybeToRight (label <> " is missing")
          $ raw
          ^? nth 4 . nth ix0 . nth ix1 . _Number

instance
  FromRpc
    'SymbolsDetails
    (Map CurrencyPair CurrencyPairConf)
  where
  fromRpc (RawResponse raw) = do
    xs <-
      maybeToRight
        "Json is not an array"
        $ raw
        ^? _Array
    res <-
      foldrM parser mempty
        $ V.filter
          ( \x ->
              (length <$> x ^? key "pair" . _String) == Just 6
          )
          xs
    if null res
      then Left "SymbolsDetails are empty"
      else pure res
    where
      parser x acc = do
        (sym, cfg) <- parseEntry x
        pure $ Map.insert sym cfg acc
      parseEntry x = do
        sym0 <-
          maybeToRight "Symbol is missing"
            $ x
            ^? key "pair" . _String
        sym <-
          first (const $ "Symbol is invalid " <> inspect sym0)
            $ newCurrencyPair sym0
        prec <-
          maybeToRight "Precision is missing"
            $ x
            ^? key "price_precision" . _Integral
        initMargin0 <-
          maybeToRight "Init Margin is missing"
            $ x
            ^? key "initial_margin" . _String
        initMargin <-
          first
            ( const
                $ "Init Margin is invalid "
                <> inspect initMargin0
            )
            $ parseRatio initMargin0
        minMargin0 <-
          maybeToRight "Min Margin is missing"
            $ x
            ^? key "minimum_margin" . _String
        minMargin <-
          first
            ( const
                $ "Min Margin is invalid "
                <> inspect minMargin0
            )
            $ parseRatio minMargin0
        maxOrderAmt0 <-
          maybeToRight "Max Order Size is missing"
            $ x
            ^? key "maximum_order_size" . _String
        maxOrderAmt <-
          first
            ( const
                $ "Max Order Size is invalid "
                <> inspect maxOrderAmt0
            )
            $ parseRatio maxOrderAmt0
        minOrderAmt0 <-
          maybeToRight "Min Order Size is missing"
            $ x
            ^? key "minimum_order_size" . _String
        minOrderAmt <-
          first
            ( const
                $ "Min Order Size is invalid "
                <> inspect minOrderAmt0
            )
            $ parseRatio minOrderAmt0
        pure
          ( sym,
            CurrencyPairConf
              { currencyPairPrecision = prec,
                currencyPairInitMargin = initMargin,
                currencyPairMinMargin = minMargin,
                currencyPairMaxOrderAmt = newMoney maxOrderAmt,
                currencyPairMinOrderAmt = newMoney minOrderAmt
              }
          )

instance
  FromRpc
    'Wallets
    ( Map
        CurrencyCode
        ( Map
            Wallets.WalletType
            Wallets.Response
        )
    )
  where
  fromRpc (RawResponse raw) = do
    xs <-
      maybeToRight
        "Json is not an array"
        $ raw
        ^? _Array
    foldrM parser mempty xs
    where
      parser x acc = do
        (currency, walletType, res) <- parseEntry x
        pure
          $ Map.alter
            ( Just
                . Map.insert walletType res
                . fromMaybe mempty
            )
            currency
            acc
      parseEntry x = do
        walletType <-
          first inspect
            . Wallets.newWalletType
            =<< maybeToRight
              "WalletType is missing"
              (x ^? nth 0 . _String)
        currency <-
          first inspect
            . newCurrencyCode
            =<< maybeToRight
              "CurrencyCode is missing"
              (x ^? nth 1 . _String)
        balance <-
          first inspect
            . roundMoney
            . newMoney
            . unsafeFrom @Rational @(Ratio Natural)
            =<< maybeToRight
              "Balance is missing"
              (toRational <$> x ^? nth 2 . _Number)
        unsettledInterest <-
          first inspect
            . roundMoney
            . newMoney
            . unsafeFrom @Rational @(Ratio Natural)
            =<< maybeToRight
              "UnsettledBalance is missing"
              (toRational <$> x ^? nth 3 . _Number)
        availableBalance <-
          first inspect
            . roundMoney
            . newMoney
            . unsafeFrom @Rational @(Ratio Natural)
            =<< maybeToRight
              "AvailableBalance is missing"
              (toRational <$> x ^? nth 4 . _Number)
        pure
          ( currency,
            walletType,
            Wallets.Response
              { Wallets.balance = balance,
                Wallets.unsettledInterest = unsettledInterest,
                Wallets.availableBalance = availableBalance,
                Wallets.lastChange = x ^? nth 5 . _String
              }
          )

instance FromRpc 'CandlesLast Candle where
  fromRpc (RawResponse raw) =
    parseCandle raw

instance FromRpc 'CandlesHist (NonEmpty Candle) where
  fromRpc (RawResponse raw) = do
    xs0 <-
      maybeToRight "Json is not an array"
        $ raw
        ^? _Array
    xs1 <-
      sortOn candleAt
        <$> mapM parseCandle (V.toList xs0)
    maybe
      (Left "Empty CandlesHist")
      pure
      $ nonEmpty xs1

instance FromRpc 'Tickers (Map CurrencyPair Ticker) where
  fromRpc (RawResponse raw) = do
    xs <-
      maybeToRight
        "Json is not an array"
        $ raw
        ^? _Array
    res <-
      foldrM parser mempty
        $ V.filter
          ( \x ->
              maybe
                False
                ((== "t") . fst . T.splitAt 1)
                (x ^? nth 0 . _String)
                && maybe
                  False
                  (>= 0)
                  (toRational <$> x ^? nth 8 . _Number)
          )
          xs
    if null res
      then Left "Tickers are empty"
      else pure res
    where
      parser x acc = do
        (k, v) <- parseEntry x
        pure $ Map.insert k v acc
      parseEntry x = do
        sym <-
          first inspect
            . newCurrencyPair
            =<< maybeToRight
              "CurrencyPair is missing"
              (x ^? nth 0 . _String)
        bid <-
          first inspect
            . roundQuotePerBase
            . newMoney
            . unsafeFrom @Rational @(Ratio Natural)
            =<< maybeToRight
              "Bid is missing"
              (toRational <$> x ^? nth 1 . _Number)
        ask0 <-
          first inspect
            . roundQuotePerBase
            . newMoney
            . unsafeFrom @Rational @(Ratio Natural)
            =<< maybeToRight
              "Ask is missing"
              (toRational <$> x ^? nth 3 . _Number)
        vol <-
          first inspect
            . roundMoney
            . newMoney
            . unsafeFrom @Rational @(Ratio Natural)
            =<< maybeToRight
              "Volume is missing"
              (toRational <$> x ^? nth 8 . _Number)
        pure
          ( sym,
            Ticker
              { tickerSymbol = sym,
                tickerVolume = vol,
                tickerBid = bid,
                tickerAsk = ask0
              }
          )
