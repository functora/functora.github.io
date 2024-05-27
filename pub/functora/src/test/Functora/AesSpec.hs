module Functora.AesSpec (spec) where

import qualified Codec.Utils as Utils
import qualified Data.ByteString.Base16 as B16
import Functora.Aes
import Functora.Prelude
import Test.Hspec
import Test.QuickCheck (arbitrary, choose, forAll, vectorOf)
import Test.QuickCheck.Instances ()

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
  it "Word128"
    $ forAll
      ( do
          size <- choose (0, 16)
          vectorOf size arbitrary
      )
      ( \xs -> do
          unsafeWord @Word128 xs
            `shouldBe` Utils.fromOctets (256 :: Integer) xs
      )
  it "Word192"
    $ forAll
      ( do
          size <- choose (0, 24)
          vectorOf size arbitrary
      )
      ( \xs -> do
          unsafeWord @Word192 xs
            `shouldBe` Utils.fromOctets (256 :: Integer) xs
      )
  it "Word256"
    $ forAll
      ( do
          size <- choose (0, 32)
          vectorOf size arbitrary
      )
      ( \xs -> do
          unsafeWord @Word256 xs
            `shouldBe` Utils.fromOctets (256 :: Integer) xs
      )
  it "zeros" $ do
    bs <-
      either throwString pure
        $ B16.decode
          "000000000000000202000000000000000649737375657200000000000000064973737565720101020000000000000009416c696365204c4c43000000000000000009416c696365204c4c43010101000000020000000000000006436c69656e740000000000000006436c69656e740101020000000000000003426f62000000000000000003426f6201010100000000000000000000010000000000000000033130300000000064000000000101010200000000000000000000000000000000010100000000000000037573640000000000000009555320446f6c6c61720000000000000000020200000000000000085175616e7469747900000000000000085175616e74697479010100000000000000000131010000000001000000000101010100000002000000000000000b4465736372697074696f6e000000000000000b4465736372697074696f6e01010200000000000000054a65616e730000000000000000054a65616e73010101000000010000000000000001000000000000000001310000000001000000000101010200000000000000000000000000000000010100000000000000036274630000000000000007426974636f696e00000000000000000202000000000000000741646472657373000000000000000741646472657373010102000000000000000000000000000000000001010100000002000000000000000744657461696c73000000000000000744657461696c73010102000000000000000000000000000000000001010100000001010808080808080808"
    km <- randomKm 32
    let aes = drvSomeAesKey @Word256 km
    unHmacDecrypt aes (encryptHmac aes bs) `shouldBe` Just bs
