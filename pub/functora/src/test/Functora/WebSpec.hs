module Functora.WebSpec (spec) where

import Functora.Prelude
import qualified Functora.Rfc2397 as Rfc2397
import Test.Hspec
import Test.QuickCheck.Instances ()

dot :: String
dot =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg=="

spec :: Spec
spec = do
  it "encode/decode" $
    fmap (uncurry Rfc2397.encode) (Rfc2397.decode dot)
      `shouldBe` Just dot
