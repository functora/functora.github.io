module Functora.Rates
  ( fetchCurrencies,
    fetchCurrencies',
    fetchQuotesPerBase,
    mkCurrenciesUris,
    mkRatesUris,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Map as Map
import qualified Data.Money as D
import Functora.Money
import Functora.Prelude
import Functora.Web
import qualified Text.URI as URI
import qualified Text.URI.Lens as URILens

fetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m (NonEmpty Currency)
fetchCurrencies = do
  uris <- mkCurrenciesUris
  eitherM throw pure $ altM fetchCurrencies' uris

fetchCurrencies' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  URI ->
  m (Either SomeException (NonEmpty Currency))
fetchCurrencies' uri = handleAny (pure . Left) $ do
  bytes <- webFetch uri mempty
  xs0 :: Map Text Text <- either throwString pure $ A.eitherDecode bytes
  xs1 <-
    maybe (throwString @Text "Zero currencies") pure . nonEmpty $ Map.toList xs0
  pure
    . Right
    . flip fmap xs1
    . uncurry
    $ \code info ->
      Currency
        { currencyCode = CurrencyCode code,
          currencyInfo = CurrencyInfo info
        }

data QuotesPerBase = QuotesPerBase
  { quotesPerBaseCreatedAt :: UTCTime,
    quotesPerBaseUpdatedAt :: UTCTime,
    quotesPerBaseQuotesMap :: Map CurrencyCode (D.Money Rational)
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

fetchQuotesPerBase ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  m QuotesPerBase
fetchQuotesPerBase base = do
  uris <- mkRatesUris base
  eitherM throw pure $ altM fetchQuotesPerBase' uris

fetchQuotesPerBase' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  URI ->
  m (Either SomeException QuotesPerBase)
fetchQuotesPerBase' uri = handleAny (pure . Left) $ do
  bytes <- webFetch uri mempty
  updatedAt <- getCurrentTime
  either throwString (pure . Right) $ do
    root :: A.Object <- A.eitherDecode bytes
    createdAt <- flip A.parseEither root (A..: "date")
    pure
      QuotesPerBase
        { quotesPerBaseCreatedAt = UTCTime {utctDay = createdAt, utctDayTime = 0},
          quotesPerBaseUpdatedAt = updatedAt,
          quotesPerBaseQuotesMap = mempty
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

mkRatesUris :: (MonadThrow m) => CurrencyCode -> m (NonEmpty URI)
mkRatesUris cur = do
  uris <- mkRootUris
  fmap sconcat . forM uris $ \uri -> do
    pre <- URI.mkPathPiece "currencies"
    pp0 <- URI.mkPathPiece $ unCurrencyCode cur <> ".min.json"
    pp1 <- URI.mkPathPiece $ unCurrencyCode cur <> ".json"
    pure
      [ uri & URILens.uriPath %~ (<> [pre, pp0]),
        uri & URILens.uriPath %~ (<> [pre, pp1])
      ]
