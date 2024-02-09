module Functora.Rates
  ( Market (..),
    MarketFailure (..),
    withMarket,
    withMarket',
    getQuote,
    getMarket,
    getCurrencies,
    getQuotesPerBase,
    Currencies (..),
    fetchCurrencies,
    fetchCurrencies',
    QuotesPerBase (..),
    fetchQuotesPerBase,
    fetchQuotesPerBase',
    mkCurrenciesUris,
    mkQuotePerBaseUris,
  )
where

import qualified Data.Aeson.Combinators.Decode as A
import qualified Data.Map as Map
import qualified Data.Money as D
import Functora.Money
import Functora.Prelude
import Functora.Web
import qualified Text.URI as URI
import qualified Text.URI.Lens as URILens

--
-- TODO : better naming for QuotesPerBase
-- better naming for safe-unsafe stuff
-- better naming for Money
--

data Market = Market
  { marketCurrencies :: Currencies,
    marketQuotesPerBase :: Map CurrencyCode QuotesPerBase
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data MarketFailure
  = MarketFailureInternal Text
  | MarketFailureMissingPair CurrencyCode CurrencyCode
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

instance Exception MarketFailure

withMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Maybe Market ->
  ReaderT (MVar Market) m a ->
  m (Either MarketFailure a)
withMarket mst expr =
  handleAny (pure . Left . MarketFailureInternal . inspect @Text)
    . fmap Right
    $ withMarket' mst expr

withMarket' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Maybe Market ->
  ReaderT (MVar Market) m a ->
  m a
withMarket' mst expr = do
  st <- maybe (Market <$> fetchCurrencies <*> pure mempty) pure mst
  mvar <- liftIO $ newMVar st
  runReaderT expr mvar

data Quote = Quote
  { quoteMoneyAmount :: D.Money Rational,
    quoteUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

getQuote ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Money ->
  CurrencyCode ->
  ReaderT (MVar Market) m (Either MarketFailure Quote)
getQuote baseMoney quoteCurrency =
  handleAny (pure . Left . MarketFailureInternal . inspect @Text) $ do
    let baseCurrency = moneyCurrencyCode baseMoney
    quotes <- getQuotesPerBase baseCurrency
    pure $ case Map.lookup quoteCurrency $ quotesPerBaseQuotesMap quotes of
      Nothing ->
        Left $ MarketFailureMissingPair baseCurrency quoteCurrency
      Just quotesPerBase ->
        Right
          Quote
            { quoteMoneyAmount = moneyAmount baseMoney D.$* quotesPerBase,
              quoteUpdatedAt = quotesPerBaseUpdatedAt quotes
            }

getMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ReaderT (MVar Market) m Market
getMarket = do
  mvar <- ask
  readMVar mvar

getCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ReaderT (MVar Market) m Currencies
getCurrencies = do
  mvar <- ask
  modifyMVar mvar $ \st -> do
    ct <- getCurrentTime
    let prev = marketCurrencies st
    if upToDate ct $ currenciesUpdatedAt prev
      then pure (st, prev)
      else do
        next <- fetchCurrencies
        pure (st {marketCurrencies = next}, next)

getQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  ReaderT (MVar Market) m QuotesPerBase
getQuotesPerBase cur = do
  mvar <- ask
  modifyMVar mvar $ \st -> do
    let quotes = marketQuotesPerBase st
    let update = do
          next <- fetchQuotesPerBase cur
          pure (st {marketQuotesPerBase = Map.insert cur next quotes}, next)
    case Map.lookup cur quotes of
      Nothing -> update
      Just prev -> do
        ct <- getCurrentTime
        if upToDate ct $ quotesPerBaseUpdatedAt prev
          then pure (st, prev)
          else update

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . nominalDiffTimeToSeconds $ diffUTCTime lhs rhs

data Currencies = Currencies
  { currenciesList :: NonEmpty CurrencyInfo,
    currenciesUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

fetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m Currencies
fetchCurrencies = do
  uris <- mkCurrenciesUris
  eitherM throw pure $ altM fetchCurrencies' uris

fetchCurrencies' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  URI ->
  m (Either SomeException Currencies)
fetchCurrencies' uri = handleAny (pure . Left) $ do
  raw <- webFetch uri mempty
  xs0 <- either throwString pure $ A.eitherDecode (A.mapStrict A.text) raw
  xs1 <-
    maybe (throwString @Text "Zero currencies") pure . nonEmpty $ Map.toList xs0
  let xs2 =
        flip fmap xs1 . uncurry $ \code text ->
          CurrencyInfo
            { currencyInfoCode = CurrencyCode code,
              currencyInfoText = text
            }
  ct <- getCurrentTime
  pure $ Right Currencies {currenciesList = xs2, currenciesUpdatedAt = ct}

data QuotesPerBase = QuotesPerBase
  { quotesPerBaseQuotesMap :: Map CurrencyCode Rational,
    quotesPerBaseCreatedAt :: UTCTime,
    quotesPerBaseUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

fetchQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  m QuotesPerBase
fetchQuotesPerBase cur = do
  uris <- mkQuotePerBaseUris cur
  eitherM throw pure $ altM (fetchQuotesPerBase' cur) uris

fetchQuotesPerBase' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  URI ->
  m (Either SomeException QuotesPerBase)
fetchQuotesPerBase' cur uri = handleAny (pure . Left) $ do
  bytes <- webFetch uri mempty
  updatedAt <- getCurrentTime
  either throwString (pure . Right) . flip A.eitherDecode bytes $ do
    createdAt <- A.at ["date"] A.day
    quotesMap <-
      A.at [fromString . from @Text @String $ unCurrencyCode cur]
        $ A.mapStrict unJsonRational
    pure
      QuotesPerBase
        { quotesPerBaseQuotesMap = Map.mapKeys CurrencyCode quotesMap,
          quotesPerBaseCreatedAt = UTCTime {utctDay = createdAt, utctDayTime = 0},
          quotesPerBaseUpdatedAt = updatedAt
        }

mkRootUris :: (MonadThrow m) => m (NonEmpty URI)
mkRootUris =
  mapM
    mkURI
    [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api@1/latest",
      "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest"
    ]

mkCurrenciesUris :: (MonadThrow m) => m (NonEmpty URI)
mkCurrenciesUris = do
  uris <- mkRootUris
  fmap sconcat . forM uris $ \uri -> do
    pp0 <- URI.mkPathPiece "currencies.min.json"
    pp1 <- URI.mkPathPiece "currencies.json"
    pure
      [ uri & URILens.uriPath %~ (<> [pp0]),
        uri & URILens.uriPath %~ (<> [pp1])
      ]

mkQuotePerBaseUris :: (MonadThrow m) => CurrencyCode -> m (NonEmpty URI)
mkQuotePerBaseUris cur = do
  uris <- mkRootUris
  fmap sconcat . forM uris $ \uri -> do
    pre <- URI.mkPathPiece "currencies"
    pp0 <- URI.mkPathPiece $ unCurrencyCode cur <> ".min.json"
    pp1 <- URI.mkPathPiece $ unCurrencyCode cur <> ".json"
    pure
      [ uri & URILens.uriPath %~ (<> [pre, pp0]),
        uri & URILens.uriPath %~ (<> [pre, pp1])
      ]
