{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.CancelOrderMultiSpec
  ( spec,
  )
where

import BitfinexClient.Data.CancelOrderMulti
import BitfinexClient.Import
import qualified Data.Aeson as A
import Test.Hspec

spec :: Spec
spec =
  describe "ToJSON" $ do
    it "ByOrderId" $
      A.encode (ByOrderId [OrderId 1, OrderId 23])
        `shouldBe` "{\"id\":[1,23]}"
    it "ByOrderClientId" $
      A.encode
        ( ByOrderClientId
            [ (OrderClientId 1, epoch),
              (OrderClientId 23, addUTCTime 86400 epoch)
            ]
        )
        `shouldBe` "{\"cid\":[[1,\"1970-01-01\"],[23,\"1970-01-02\"]]}"
    it "ByOrderGroupId" $
      A.encode (ByOrderGroupId [OrderGroupId 1, OrderGroupId 23])
        `shouldBe` "{\"gid\":[1,23]}"
    it "Everything" $
      A.encode Everything
        `shouldBe` "{\"all\":1}"
