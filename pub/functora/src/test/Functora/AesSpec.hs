module Functora.AesSpec (spec) where

import Functora.Aes
import Functora.Prelude
import Test.Hspec

sample :: ByteString
sample = "Hello, World!"

spec :: Spec
spec = do
  it "encrypt/decrypt" $ do
    ikm <- Ikm <$> randomByteString 32
    salt <- Salt <$> randomByteString 32
    info <- Info <$> randomByteString 32
    let aes0 = drvSomeAesKey @Word256 ikm salt info
    let aes1 = drvSomeAesKey @Word256 (Ikm "User defined key") salt info
    decrypt aes0 (encrypt aes0 sample) `shouldBe` Just sample
    decrypt aes1 (encrypt aes1 sample) `shouldBe` Just sample
    decrypt aes1 (encrypt aes0 sample) `shouldNotBe` Just sample
    decrypt aes0 (encrypt aes1 sample) `shouldNotBe` Just sample
