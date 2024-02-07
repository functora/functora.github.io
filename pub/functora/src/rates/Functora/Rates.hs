module Functora.Rates
  ( fetchCurrencies,
    fetchCurrencies',
    mkRootUris,
    mkCurrenciesUris,
    mkRatesUris,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map as Map
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

mkRatesUris :: (MonadThrow m) => Currency -> m (NonEmpty URI)
mkRatesUris cur = do
  uris <- mkRootUris
  let code = unCurrencyCode $ currencyCode cur
  fmap sconcat . forM uris $ \uri -> do
    pre <- URI.mkPathPiece "currencies"
    pp0 <- URI.mkPathPiece $ code <> ".min.json"
    pp1 <- URI.mkPathPiece $ code <> ".json"
    pure
      [ uri & URILens.uriPath %~ (<> [pre, pp0]),
        uri & URILens.uriPath %~ (<> [pre, pp1])
      ]
