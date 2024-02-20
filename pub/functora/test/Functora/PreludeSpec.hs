module Functora.PreludeSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Functora.Prelude
import Test.Hspec
import qualified Universum

newtype Buz = Buz
  { _unBuz :: Map ByteString Int
  }
  deriving stock (Eq, Ord, Show, Data)

spec :: Spec
spec = do
  it "inspect/data" $ do
    let x = Buz [("buf", 123), (BS.pack [106, 246, 171, 231, 231, 166, 121], 321)]
    Universum.show @Text x
      `shouldBe` "Buz {_unBuz = fromList [(\"buf\",123),(\"j\\246\\171\\231\\231\\166y\",321)]}"
    inspect @Text x
      `shouldBe` "Buz {_unBuz = fromList [(\"6af6abe7e7a679\",321),(\"buf\",123)]}"
  it "inspect/IsString" $ do
    inspect @Text ("HELLO" :: String) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: Text) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: TL.Text) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: ByteString) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: BL.ByteString) `shouldBe` "HELLO"
  it "inspectRatio" $ do
    let samples :: [(Rational, Text)] =
          [ (0, "0"),
            (1, "1"),
            (42, "42"),
            (0.3, "0.3"),
            (0.33, "0.33"),
            (0.333, "0.333"),
            (0.3333, "0.333"),
            (0.33333, "0.333"),
            (1 % 3, "0.333")
          ]
    forM_ samples
      . uncurry
      $ \lhs rhs -> inspectRatio defaultRatioFormat lhs `shouldBe` rhs
