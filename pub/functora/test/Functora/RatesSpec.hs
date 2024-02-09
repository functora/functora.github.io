module Functora.RatesSpec (spec) where

import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude
import Functora.Rates
import Test.Hspec
import qualified Text.URI as URI

spec :: Spec
spec = do
  it "fetchCurrencies" $ do
    currencies <- fetchCurrencies
    let cur = find (\x -> currencyInfoCode x == CurrencyCode "btc") currencies
    cur `shouldSatisfy` isJust
  it "fetchCurrencies'" $ do
    uris <- mkCurrenciesUris
    forM_ uris $ \uri -> do
      res <- fetchCurrencies' uri
      shouldSatisfy res $ \case
        Left {} -> False
        Right currencies ->
          isJust $ find (\x -> currencyInfoCode x == CurrencyCode "btc") currencies
  it "fetchQuotesPerBase" $ do
    res <- quotesPerBaseQuotesMap <$> fetchQuotesPerBase (CurrencyCode "btc")
    Map.lookup (CurrencyCode "usd") res `shouldSatisfy` isJust
  it "mkCurrenciesUris" $ do
    lhs <- URI.render <<$>> mkCurrenciesUris
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies.min.json",
            "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies.min.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies.json"
          ]
    lhs `shouldBe` rhs
  it "mkRatesUris" $ do
    lhs <- URI.render <<$>> mkRatesUris (CurrencyCode "btc")
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies/btc.min.json",
            "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies/btc.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies/btc.min.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies/btc.json"
          ]
    lhs `shouldBe` rhs
