module Functora.Rates
  ( -- * State
    -- $state
    Market (..),
    newMarket,
    withMarket,
    withNewMarket,

    -- * Stateful
    -- $stateful
    QuoteAt (..),
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
    QuotesPerBaseAt (..),
    fetchQuotesPerBase,
    tryFetchQuotesPerBase,

    -- * Uris
    -- $uris
    newCurrenciesUris,
    newQuotePerBaseUris,

    -- * Exceptions
    -- $exceptions
    MarketException (..),
    tryMarket,
  )
where

import qualified Data.Aeson.Combinators.Decode as A
import qualified Data.Map as Map
import Functora.Cfg
import Functora.Money
import Functora.Prelude
import Functora.Web
import qualified Text.URI as URI
import qualified Text.URI.Lens as URILens

-- $state
-- State

data Market = Market
  { marketCurrencies :: Currencies,
    marketQuotesPerBase :: Map CurrencyCode QuotesPerBaseAt
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (Binary, FromJSON, ToJSON) via GenericType Market

newMarket :: (MonadIO m) => m (MVar Market)
newMarket = newEmptyMVar

withMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  MVar Market ->
  ReaderT (MVar Market) m a ->
  m a
withMarket opts var expr = do
  mst <- tryReadMVar var
  when (isNothing mst) $ do
    st <- Market <$> fetchCurrencies opts <*> pure mempty
    void $ tryPutMVar var st
  runReaderT expr var

withNewMarket ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  ReaderT (MVar Market) m a ->
  m a
withNewMarket opts expr = do
  var <- newMarket
  withMarket opts var expr

-- $stateful
-- Stateful

data QuoteAt = QuoteAt
  { quoteMoneyAmount :: Money (Tags 'Signed |+| 'Quote |+| 'MoneyAmount),
    quoteUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (Binary, FromJSON, ToJSON) via GenericType QuoteAt

getQuote ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  Funds (Tags 'Signed |+| 'Base |+| 'MoneyAmount) ->
  CurrencyCode ->
  ReaderT (MVar Market) m QuoteAt
getQuote opts baseFunds quoteCurrency = do
  let baseCurrency = fundsCurrencyCode baseFunds
  quotes <- getQuotesPerBase opts baseCurrency
  case Map.lookup quoteCurrency $ quotesPerBaseQuotesMap quotes of
    Nothing ->
      throw
        $ MarketExceptionMissingBaseAndQuote baseCurrency quoteCurrency
    Just quotesPerBase ->
      pure
        QuoteAt
          { quoteMoneyAmount =
              exchangeMoney @(Tags 'Signed) quotesPerBase
                $ fundsMoneyAmount baseFunds,
            quoteUpdatedAt =
              quotesPerBaseUpdatedAt quotes
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
  Opts ->
  ReaderT (MVar Market) m Currencies
getCurrencies opts = do
  var <- ask
  modifyMVar var $ \st -> do
    ct <- getCurrentTime
    let prev = marketCurrencies st
    if upToDate ct $ currenciesUpdatedAt prev
      then pure (st, prev)
      else do
        next <- fetchCurrencies opts
        pure (st {marketCurrencies = next}, next)

getCurrencyInfo ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  CurrencyCode ->
  ReaderT (MVar Market) m CurrencyInfo
getCurrencyInfo opts code = do
  xs <- currenciesList <$> getCurrencies opts
  case find ((== code) . currencyInfoCode) xs of
    Nothing -> throw $ MarketExceptionMissingCurrencyCode code
    Just x -> pure x

getQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  CurrencyCode ->
  ReaderT (MVar Market) m QuotesPerBaseAt
getQuotesPerBase opts cur = do
  var <- ask
  modifyMVar var $ \st -> do
    let quotes = marketQuotesPerBase st
    let update = do
          next <- fetchQuotesPerBase opts cur
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
  deriving (Binary, FromJSON, ToJSON) via GenericType Currencies

fetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  m Currencies
fetchCurrencies opts = do
  uris <- newCurrenciesUris
  eitherM throw pure $ altM (tryFetchCurrencies opts) uris

tryFetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  URI ->
  m (Either MarketException Currencies)
tryFetchCurrencies opts uri = tryMarket $ do
  raw <- utf8FromLatin1 <$> webFetch uri opts
  xs0 <-
    either throwString pure $ A.eitherDecode (A.mapStrict A.text) raw
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

data QuotesPerBaseAt = QuotesPerBaseAt
  { quotesPerBaseQuotesMap ::
      Map CurrencyCode (Money (Tags 'Signed |+| 'QuotePerBase)),
    quotesPerBaseCreatedAt :: UTCTime,
    quotesPerBaseUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (Binary, FromJSON, ToJSON) via GenericType QuotesPerBaseAt

fetchQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  CurrencyCode ->
  m QuotesPerBaseAt
fetchQuotesPerBase opts cur = do
  uris <- newQuotePerBaseUris cur
  eitherM throw pure $ altM (tryFetchQuotesPerBase opts cur) uris

tryFetchQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Opts ->
  CurrencyCode ->
  URI ->
  m (Either MarketException QuotesPerBaseAt)
tryFetchQuotesPerBase opts cur uri = tryMarket $ do
  bytes <- utf8FromLatin1 <$> webFetch uri opts
  updatedAt <- getCurrentTime
  either throwString pure . flip A.eitherDecode bytes $ do
    createdAt <- A.at ["date"] A.day
    quotesMap <-
      A.at [fromString . from @Text @String $ unCurrencyCode cur]
        $ A.mapStrict unJsonMoney
    pure
      QuotesPerBaseAt
        { quotesPerBaseQuotesMap = Map.mapKeys CurrencyCode quotesMap,
          quotesPerBaseCreatedAt = UTCTime {utctDay = createdAt, utctDayTime = 0},
          quotesPerBaseUpdatedAt = updatedAt
        }

-- $uris
-- Uris

newRootUris :: (MonadThrow m) => m (NonEmpty URI)
newRootUris =
  mapM
    mkURI
    [ "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@latest/v1"
    ]

newCurrenciesUris :: (MonadThrow m) => m (NonEmpty URI)
newCurrenciesUris = do
  uris <- newRootUris
  fmap sconcat . forM uris $ \uri -> do
    pp0 <- URI.mkPathPiece "currencies.min.json"
    pp1 <- URI.mkPathPiece "currencies.json"
    pure
      [ uri & URILens.uriPath %~ (<> [pp0]),
        uri & URILens.uriPath %~ (<> [pp1])
      ]

newQuotePerBaseUris :: (MonadThrow m) => CurrencyCode -> m (NonEmpty URI)
newQuotePerBaseUris cur = do
  uris <- newRootUris
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
