module Functora.RatesSpec (spec) where

import Functora.Money
import Functora.Prelude
import Functora.Rates
import Test.Hspec
import qualified Text.URI as URI

spec :: Spec
spec = do
  it "mkCurrenciesUris" $ do
    lhs <- URI.render <<$>> mkCurrenciesUris
    let rhs :: NonEmpty Text =
          [ "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies.min.json",
            "https://cdn.jsdelivr.net/gh/fawazahmed0/currency-api%401/latest/currencies.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies.min.json",
            "https://raw.githubusercontent.com/fawazahmed0/currency-api/1/latest/currencies.json"
          ]
    lhs `shouldBe` rhs
  it "fetchCurrencies" $ do
    currencies <- fetchCurrencies
    let cur = find (\x -> currencyCode x == "usd") currencies
    cur `shouldSatisfy` isJust
  it "fetchCurrencies'" $ do
    uris <- mkCurrenciesUris
    forM_ uris $ \uri -> do
      res <- fetchCurrencies' uri
      shouldSatisfy res $ \case
        Left {} -> False
        Right currencies ->
          isJust $ find (\x -> currencyCode x == "usd") currencies
