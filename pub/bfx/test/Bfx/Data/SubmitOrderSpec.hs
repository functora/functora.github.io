{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.SubmitOrderSpec
  ( spec,
  )
where

import Bfx
import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.TestEnv
import qualified Data.Aeson as A
import Functora.Money
import Functora.Prelude
import Test.Hspec

spec :: Spec
spec =
  before sysEnv $ describe "ToJSON" $ do
    it "Request" . const $ do
      let req =
            SubmitOrder.Request
              Buy
              testAdaAmt
              adaBtc
              (QuotePerBase 0.00081037)
              SubmitOrder.optsPostOnly
      A.encode req
        `shouldBe` "{\"amount\":\"4.004004\",\"flags\":4096,\"price\":\"0.00081037\",\"symbol\":\"tADABTC\",\"type\":\"EXCHANGE LIMIT\"}"
