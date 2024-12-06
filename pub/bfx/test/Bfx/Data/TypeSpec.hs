{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.TypeSpec
  ( spec,
  )
where

import Bfx.Import
import Test.Hspec

spec :: Spec
spec = do
  describe "Orders" $ do
    it "unOrderFlag succeeds"
      $ unOrderFlag PostOnly
      `shouldBe` OrderFlagAcc 4096
    it "unOrderFlagSet succeeds"
      $ unOrderFlagSet [Hidden, PostOnly]
      `shouldBe` OrderFlagAcc 4160
  describe "Trading" $ do
    it "currencyPairCon succeeds"
      $ currencyPairCon
        (Tagged @'Base $ CurrencyCode "ADA")
        (Tagged @'Quote $ CurrencyCode "BTC")
      `shouldSatisfy` isRight
    it "currencyPairCon fails"
      $ currencyPairCon
        (Tagged @'Base $ CurrencyCode "BTC")
        (Tagged @'Quote $ CurrencyCode "BTC")
      `shouldSatisfy` isLeft
