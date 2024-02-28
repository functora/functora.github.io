{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.TypeSpec
  ( spec,
  )
where

import BitfinexClient.Import
import Test.Hspec

spec :: Spec
spec = do
  describe "Orders" $ do
    it "unOrderFlag succeeds" $
      unOrderFlag PostOnly
        `shouldBe` OrderFlagAcc 4096
    it "unOrderFlagSet succeeds" $
      unOrderFlagSet [Hidden, PostOnly]
        `shouldBe` OrderFlagAcc 4160
  describe "Trading" $ do
    it "currencyPairCon succeeds" $
      currencyPairCon [ccBase|ADA|] [ccQuote|BTC|]
        `shouldSatisfy` isRight
    it "currencyPairCon fails" $
      currencyPairCon [ccBase|BTC|] [ccQuote|BTC|]
        `shouldSatisfy` isLeft
