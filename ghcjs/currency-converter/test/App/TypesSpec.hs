module App.TypesSpec (spec) where

import qualified Data.Aeson as A
import Functora.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  prop "Identity/JSON" $ \txt -> do
    let wrap :: Identity Text = Identity txt
    let txtJson = A.encode txt
    let wrapJson = A.encode wrap
    wrap ^. #runIdentity `shouldBe` txt
    txtJson `shouldBe` wrapJson
    A.decode txtJson `shouldBe` Just txt
    A.decode wrapJson `shouldBe` Just wrap
