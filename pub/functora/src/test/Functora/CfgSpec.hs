{-# LANGUAGE QuasiQuotes #-}

module Functora.CfgSpec (spec) where

import Functora.Cfg
import Functora.Prelude
import Test.Hspec
import Text.URI.QQ

newtype Buz = Buz
  { _buzUri :: URI
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

spec :: Spec
spec = do
  it "URI/TOML" $ do
    decodeToml "uri = \"http://hello.world\""
      `shouldBe` Right (Buz [uri|http://hello.world|])
