module Functora.AesSpec (spec) where

import Functora.Aes
import Functora.Prelude
import Test.Hspec

sample :: ByteString
sample = "Hello, World!"

spec :: Spec
spec = do
  it "encryptHmac/unHmacDecrypt" $ do
    hkdf <- randomHkdf 32
    let aes0 = drvSomeAesKey @Word256 hkdf
    let aes1 = drvSomeAesKey @Word256 hkdf {hkdfIkm = Ikm "User defined key"}
    unHmacDecrypt aes0 (encryptHmac aes0 sample) `shouldBe` Just sample
    unHmacDecrypt aes1 (encryptHmac aes1 sample) `shouldBe` Just sample
    unHmacDecrypt aes1 (encryptHmac aes0 sample) `shouldBe` Nothing
    unHmacDecrypt aes0 (encryptHmac aes1 sample) `shouldBe` Nothing
