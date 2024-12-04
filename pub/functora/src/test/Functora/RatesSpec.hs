module Functora.RatesSpec (spec) where

import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude
import Functora.Rates
import Functora.Web (defOpts)
import Test.Hspec
import qualified Text.URI as URI

spec :: Spec
spec = do
  it "fetchCurrencies" $ do
    currencies <- currenciesList <$> fetchCurrencies defOpts
    let cur = find (\x -> currencyInfoCode x == CurrencyCode "btc") currencies
    cur `shouldSatisfy` isJust
  it "tryFetchCurrencies" $ do
    uris <- newCurrenciesUris
    forM_ uris $ \uri -> do
      res <- currenciesList <<$>> tryFetchCurrencies defOpts uri
      shouldSatisfy res $ \case
        Left {} -> False
        Right currencies ->
          isJust $ find (\x -> currencyInfoCode x == CurrencyCode "btc") currencies
  it "fetchQuotesPerBase" $ do
    res <-
      quotesPerBaseQuotesMap <$> fetchQuotesPerBase defOpts (CurrencyCode "btc")
    Map.lookup (CurrencyCode "usd") res `shouldSatisfy` isJust
  it "tryFetchQuotesPerBase" $ do
    let cur = CurrencyCode "btc"
    uris <- newQuotePerBaseUris cur
    forM_ uris $ \uri -> do
      res <- quotesPerBaseQuotesMap <<$>> tryFetchQuotesPerBase defOpts cur uri
      shouldSatisfy res $ \case
        Left {} -> False
        Right xs -> isJust $ Map.lookup (CurrencyCode "usd") xs
  it "newCurrenciesUris" $ do
    lhs <- URI.render <<$>> newCurrenciesUris
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/npm/%40fawazahmed0/currency-api%40latest/v1/currencies.min.json",
            "https://cdn.jsdelivr.net/npm/%40fawazahmed0/currency-api%40latest/v1/currencies.json"
          ]
    lhs `shouldBe` rhs
  it "newQuotePerBaseUris" $ do
    lhs <- URI.render <<$>> newQuotePerBaseUris (CurrencyCode "btc")
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/npm/%40fawazahmed0/currency-api%40latest/v1/currencies/btc.min.json",
            "https://cdn.jsdelivr.net/npm/%40fawazahmed0/currency-api%40latest/v1/currencies/btc.json"
          ]
    lhs `shouldBe` rhs
  it "getCurrencies" . withNewMarket defOpts $ do
    lhs <- getCurrencies defOpts
    rhs <- getCurrencies defOpts
    lift $ lhs `shouldBe` rhs
  it "getQuotesPerBase" . withNewMarket defOpts $ do
    let cur = CurrencyCode "btc"
    lhs <- getQuotesPerBase defOpts cur
    rhs <- getQuotesPerBase defOpts cur
    lift $ lhs `shouldBe` rhs
  it "getQuote" . withNewMarket defOpts $ do
    let quoteCurrency = CurrencyCode "usd"
    res <-
      tryMarket
        $ getQuote
          defOpts
          Money
            { moneyAmount = MoneyAmount 1,
              moneyCurrencyCode = CurrencyCode "btc"
            }
          quoteCurrency
    lift $ res `shouldSatisfy` isRight
