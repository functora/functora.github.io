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
  it "inspectRatio/samples" $ do
    let samples :: [(Rational, Text)] =
          [ (0, "0"),
            (1, "1"),
            (42, "42"),
            (0.3, "0.3"),
            (0.33, "0.33"),
            (0.333, "0.333"),
            (0.3333, "0.333"),
            (0.33333, "0.333"),
            (1 % 3, "0.333"),
            (0.6, "0.6"),
            (0.66, "0.66"),
            (0.666, "0.666"),
            (0.6666, "0.667"),
            (0.66666, "0.667"),
            (2 % 3, "0.667"),
            (1.3, "1.3"),
            (1.33, "1.33"),
            (1.333, "1.33"),
            (1.3333, "1.33"),
            (1.33333, "1.33"),
            (4 % 3, "1.33"),
            (1.6, "1.6"),
            (1.66, "1.66"),
            (1.666, "1.67"),
            (1.6666, "1.67"),
            (1.66666, "1.67"),
            (5 % 3, "1.67"),
            (10003.6, "10003.6"),
            (10003.66, "10003.66"),
            (10003.666, "10003.67"),
            (10003.6666, "10003.67"),
            (10003.66666, "10003.67"),
            (30011 % 3, "10003.67"),
            (10003.06, "10003.06"),
            (10003.006, "10003.01"),
            (10003.00000006, "10003"),
            (2 % 3000, "0.000667"),
            (2 % 30000, "0.0000667"),
            (2 % 300000, "6.67e-6"),
            (2 % 3000000, "6.67e-7"),
            (2 % 30000000, "6.67e-8"),
            (0.00000001030405, "1.03e-8")
          ]
    forM_ samples
      . uncurry
      $ \lhs rhs -> do
        inspectRatio defaultRatioFormat lhs `shouldBe` rhs
        when (lhs /= 0)
          $ inspectRatio defaultRatioFormat (-lhs)
          `shouldBe` "-" <> rhs
