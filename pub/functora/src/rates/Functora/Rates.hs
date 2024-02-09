module Functora.Rates
  ( fetchCurrencies,
    fetchCurrencies',
    QuotesPerBase (..),
    fetchQuotesPerBase,
    fetchQuotesPerBase',
    mkCurrenciesUris,
    mkRatesUris,
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

fetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m (NonEmpty CurrencyInfo)
fetchCurrencies = do
  uris <- mkCurrenciesUris
  eitherM throw pure $ altM fetchCurrencies' uris

fetchCurrencies' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  URI ->
  m (Either SomeException (NonEmpty CurrencyInfo))
fetchCurrencies' uri = handleAny (pure . Left) $ do
  raw <- webFetch uri mempty
  xs0 <- either throwString pure $ A.eitherDecode (A.mapStrict A.text) raw
  xs1 <-
    maybe (throwString @Text "Zero currencies") pure . nonEmpty $ Map.toList xs0
  pure
    . Right
    . flip fmap xs1
    . uncurry
    $ \code text ->
      CurrencyInfo
        { currencyInfoCode = CurrencyCode code,
          currencyInfoText = text
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
fetchQuotesPerBase baseCur = do
  uris <- mkRatesUris baseCur
  eitherM throw pure $ altM (fetchQuotesPerBase' baseCur) uris

fetchQuotesPerBase' ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  CurrencyCode ->
  URI ->
  m (Either SomeException QuotesPerBase)
fetchQuotesPerBase' baseCur uri = handleAny (pure . Left) $ do
  bytes <- webFetch uri mempty
  updatedAt <- getCurrentTime
  either throwString (pure . Right) . flip A.eitherDecode bytes $ do
    createdAt <- A.at ["date"] A.day
    quotesMap <-
      A.at [fromString . from @Text @String $ unCurrencyCode baseCur]
        $ A.mapStrict unJsonMoneyAmount
    pure
      QuotesPerBase
        { quotesPerBaseCreatedAt = UTCTime {utctDay = createdAt, utctDayTime = 0},
          quotesPerBaseUpdatedAt = updatedAt,
          quotesPerBaseQuotesMap = Map.mapKeys CurrencyCode quotesMap
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
