module Functora.Rates
  ( fetchCurrencies,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude
import Functora.Web

fetchCurrencies ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  m (NonEmpty Currency)
fetchCurrencies = do
  uris <-
    mapM
      mkURI
      [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api@1/latest/currencies.min.json",
        "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api@1/latest/currencies.json"
      ]
  eitherM throw pure
    $ altM fetchCurrencies' uris

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
    $ flip fmap xs1
    $ \(code, info) ->
      Currency
        { currencyCode = code,
          currencyInfo = info
        }
