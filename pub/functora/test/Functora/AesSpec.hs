module Functora.AesSpec (spec) where

import Functora.Aes
import Functora.Prelude
import qualified System.Random.Stateful as Random
import Test.Hspec

sample :: ByteString
sample = "Hello, World!"

spec :: Spec
spec = do
  it "encrypt/decrypt" $ do
    ikm <- Tagged @"IKM" <$> Random.uniformByteStringM 32 Random.globalStdGen
    salt <- Tagged @"Salt" <$> Random.uniformByteStringM 32 Random.globalStdGen
    info <- Tagged @"Info" <$> Random.uniformByteStringM 32 Random.globalStdGen
    let aes0 = drvSomeAesKey @Word256 ikm salt info
    let aes1 = drvSomeAesKey @Word256 (Tagged @"IKM" "User defined key") salt info
    decrypt aes0 (encrypt aes0 sample) `shouldBe` Just sample
    decrypt aes1 (encrypt aes1 sample) `shouldBe` Just sample
    decrypt aes1 (encrypt aes0 sample) `shouldNotBe` Just sample
    decrypt aes0 (encrypt aes1 sample) `shouldNotBe` Just sample
