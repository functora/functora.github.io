module Functora.RatesSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Money as D
import Functora.Money
import Functora.Prelude
import Functora.Rates
import Test.Hspec
import qualified Text.URI as URI

spec :: Spec
spec = do
  it "fetchCurrencies" $ do
    currencies <- currenciesList <$> fetchCurrencies
    let cur = find (\x -> currencyInfoCode x == CurrencyCode "btc") currencies
    cur `shouldSatisfy` isJust
  it "tryFetchCurrencies" $ do
    uris <- mkCurrenciesUris
    forM_ uris $ \uri -> do
      res <- currenciesList <<$>> tryFetchCurrencies uri
      shouldSatisfy res $ \case
        Left {} -> False
        Right currencies ->
          isJust $ find (\x -> currencyInfoCode x == CurrencyCode "btc") currencies
  it "fetchQuotesPerBase" $ do
    res <- quotesPerBaseQuotesMap <$> fetchQuotesPerBase (CurrencyCode "btc")
    Map.lookup (CurrencyCode "usd") res `shouldSatisfy` isJust
  it "tryFetchQuotesPerBase" $ do
    let cur = CurrencyCode "btc"
    uris <- mkQuotePerBaseUris cur
    forM_ uris $ \uri -> do
      res <- quotesPerBaseQuotesMap <<$>> tryFetchQuotesPerBase cur uri
      shouldSatisfy res $ \case
        Left {} -> False
        Right xs -> isJust $ Map.lookup (CurrencyCode "usd") xs
  it "mkCurrenciesUris" $ do
    lhs <- URI.render <<$>> mkCurrenciesUris
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies.min.json",
            "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies.min.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies.json"
          ]
    lhs `shouldBe` rhs
  it "mkQuotePerBaseUris" $ do
    lhs <- URI.render <<$>> mkQuotePerBaseUris (CurrencyCode "btc")
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies/btc.min.json",
            "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies/btc.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies/btc.min.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies/btc.json"
          ]
    lhs `shouldBe` rhs
  it "getCurrencies" . withMarket Nothing $ do
    lhs <- getCurrencies
    rhs <- getCurrencies
    lift $ lhs `shouldBe` rhs
  it "getQuotesPerBase" . withMarket Nothing $ do
    let cur = CurrencyCode "btc"
    lhs <- getQuotesPerBase cur
    rhs <- getQuotesPerBase cur
    lift $ lhs `shouldBe` rhs
  it "getQuote" . withMarket Nothing $ do
    let quoteCurrency = CurrencyCode "usd"
    res <-
      tryMarket $ getQuote (Money (D.Money 1) $ CurrencyCode "btc") quoteCurrency
    lift $ res `shouldSatisfy` isRight
