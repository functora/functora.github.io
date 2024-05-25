module Functora.AesSpec (spec) where

import Functora.Aes
import Functora.Prelude
import Test.Hspec

sample :: ByteString
sample = "Hello, World!"

spec :: Spec
spec = do
  it "encryptHmac/unHmacDecrypt" $ do
    km <- randomKm 32
    let aes0 = drvSomeAesKey @Word256 km
    let aes1 = drvSomeAesKey @Word256 km {kmIkm = Ikm "User defined key"}
    unHmacDecrypt aes0 (encryptHmac aes0 sample) `shouldBe` Just sample
    unHmacDecrypt aes1 (encryptHmac aes1 sample) `shouldBe` Just sample
    unHmacDecrypt @ByteString aes1 (encryptHmac aes0 sample) `shouldBe` Nothing
    unHmacDecrypt @ByteString aes0 (encryptHmac aes1 sample) `shouldBe` Nothing
