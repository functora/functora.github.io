module Functora.AesSpec (spec) where

import Functora.Aes
import Functora.Prelude
import Test.Hspec

spec :: Spec
spec = do
  it "withBlocks" $ do
    withWs128 @ByteString id "Hello, World!"
      `shouldBe` "Hello, World!"