module Functora.Rates
  ( -- * State
    -- $state
    Market (..),
    mkMarket,
    withMarket,
    withNewMarket,

    -- * Stateful
    -- $stateful
    Quote (..),
    getQuote,
    getMarket,
    getCurrencies,
    getCurrencyInfo,
    getQuotesPerBase,

    -- * Stateless
    -- $stateless
    Currencies (..),
    fetchCurrencies,
    tryFetchCurrencies,
    QuotesPerBase (..),
    fetchQuotesPerBase,
    tryFetchQuotesPerBase,

    -- * Uris
    -- $uris
    mkCurrenciesUris,
    mkQuotePerBaseUris,

    -- * Exceptions
    -- $exceptions
    MarketException (..),
    tryMarket,
  )
where

import qualified Data.Aeson.Combinators.Decode as A
import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude
import Functora.Web
import qualified Text.URI as URI
import qualified Text.URI.Lens as URILens

-- $state
-- State

data Market = Market
  { marketCurrencies :: Currencies,
    marketQuotesPerBase :: Map CurrencyCode QuotesPerBase
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

mkMarket :: (MonadIO m) => m (MVar Market)
mkMarket = newEmptyMVar

withMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  MVar Market ->
  ReaderT (MVar Market) m a ->
  m a
withMarket var expr = do
  mst <- tryReadMVar var
  when (isNothing mst) $ do
    st <- Market <$> fetchCurrencies <*> pure mempty
    void $ tryPutMVar var st
  runReaderT expr var

withNewMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ReaderT (MVar Market) m a ->
  m a
withNewMarket expr = do
  var <- mkMarket
  withMarket var expr

-- $stateful
-- Stateful

data Quote = Quote
  { quoteMoneyAmount :: Money Rational,
    quoteUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

getQuote ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Funds ->
  CurrencyCode ->
  ReaderT (MVar Market) m Quote
getQuote baseFunds quoteCurrency = do
  let baseCurrency = fundsCurrencyCode baseFunds
  quotes <- getQuotesPerBase baseCurrency
  case Map.lookup quoteCurrency $ quotesPerBaseQuotesMap quotes of
    Nothing ->
      throw
        $ MarketExceptionMissingBaseAndQuote baseCurrency quoteCurrency
    Just quotesPerBase ->
      pure
        Quote
          { quoteMoneyAmount = fundsMoneyAmount baseFunds $* quotesPerBase,
            quoteUpdatedAt = quotesPerBaseUpdatedAt quotes
          }

getMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ReaderT (MVar Market) m Market
getMarket = do
  var <- ask
  readMVar var

getCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ReaderT (MVar Market) m Currencies
getCurrencies = do
  var <- ask
  modifyMVar var $ \st -> do
    ct <- getCurrentTime
    let prev = marketCurrencies st
    if upToDate ct $ currenciesUpdatedAt prev
      then pure (st, prev)
      else do
        next <- fetchCurrencies
        pure (st {marketCurrencies = next}, next)

getCurrencyInfo ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  ReaderT (MVar Market) m CurrencyInfo
getCurrencyInfo code = do
  xs <- currenciesList <$> getCurrencies
  case find ((== code) . currencyInfoCode) xs of
    Nothing -> throw $ MarketExceptionMissingCurrencyCode code
    Just x -> pure x

getQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  ReaderT (MVar Market) m QuotesPerBase
getQuotesPerBase cur = do
  var <- ask
  modifyMVar var $ \st -> do
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
    diff = abs . toRational $ diffUTCTime lhs rhs

-- $stateless
-- Stateless

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
  eitherM throw pure $ altM tryFetchCurrencies uris

tryFetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  URI ->
  m (Either MarketException Currencies)
tryFetchCurrencies uri = tryMarket $ do
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
  pure Currencies {currenciesList = xs2, currenciesUpdatedAt = ct}

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
  eitherM throw pure $ altM (tryFetchQuotesPerBase cur) uris

tryFetchQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  URI ->
  m (Either MarketException QuotesPerBase)
tryFetchQuotesPerBase cur uri = tryMarket $ do
  bytes <- webFetch uri mempty
  updatedAt <- getCurrentTime
  either throwString pure . flip A.eitherDecode bytes $ do
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

-- $uris
-- Uris

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

-- $exceptions
-- Exceptions

data MarketException
  = MarketExceptionInternal SomeException
  | MarketExceptionMissingCurrencyCode CurrencyCode
  | MarketExceptionMissingBaseAndQuote CurrencyCode CurrencyCode
  deriving stock (Show, Data, Generic)

instance Exception MarketException

tryMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m a ->
  m (Either MarketException a)
tryMarket =
  handleAny (pure . Left . MarketExceptionInternal)
    . handle (pure . Left)
    . fmap Right