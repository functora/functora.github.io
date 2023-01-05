{-# LANGUAGE TypeApplications #-}

module GenericPrettyInstancesSpec (spec) where

import Test.Hspec
import Text.PrettyPrint.GenericPretty.Import
import Universum hiding (show)

newtype Username = Username String
  deriving stock (Generic)

instance Out Username

data Configuration = Configuration
  { username :: Username,
    localHost :: String
  }
  deriving stock (Generic)

instance Out Configuration

spec :: Spec
spec = do
  it "Bool" $
    inspect @Text True
      `shouldBe` "True"
  it "List" $
    inspectPlain @Text ([1 .. 5] :: [Int])
      `shouldBe` "[ 1, 2, 3, 4, 5 ]"
  it "NestedRecord" $
    inspectPlain @Text
      Configuration
        { username = Username "john",
          localHost = "127.0.0.1"
        }
      `shouldBe` "Configuration    { username = Username \"john\", localHost = \"127.0.0.1\" }"
