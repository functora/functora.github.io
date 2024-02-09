module Functora.Rates
  ( Currencies (..),
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
  { quotesPerBaseQuotesMap :: Map CurrencyCode (D.Money Rational),
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
        $ A.mapStrict unJsonMoneyAmount
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
