module Functora.CfgSpec (spec) where

import Functora.Cfg
import Functora.Prelude
import Test.Hspec

newtype Buz = Buz
  { _buzUri :: URI
  }
  deriving newtype (Eq, Ord, Show)
  deriving stock (Data, Generic)

spec :: Spec
spec = do
  it "URI/TOML" $ do
    decodeToml "uri = \"http://hello.world\""
      `shouldBe` Right (Buz [uri|http://hello.world|])
