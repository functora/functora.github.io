module App.TypesSpec (spec) where

import App.Init
import App.Types
import qualified Data.Aeson as A
import Functora.Miso.Prelude hiding (prop)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  prop "Identity/JSON" $ \txt -> do
    let wrap :: Identity Unicode = Identity txt
    let txtJson = A.encode txt
    let wrapJson = A.encode wrap
    wrap ^. #runIdentity `shouldBe` txt
    txtJson `shouldBe` wrapJson
    A.decode txtJson `shouldBe` Just txt
    A.decode wrapJson `shouldBe` Just wrap
  it "serialization" $ do
    var <- newEmptyMVar
    st0 <- newModel var Nothing Nothing
    uri <- mkUri st0
    mSt <- unUri uri
    st1 <- newModel var Nothing mSt
    (st0 ^. #modelState . to uniqueToIdentity)
      `shouldBe` (st1 ^. #modelState . to uniqueToIdentity)
