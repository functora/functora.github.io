{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.SubmitOrderSpec
  ( spec,
  )
where

import qualified Bfx.Data.SubmitOrder as SubmitOrder
import Bfx.Import
import Bfx.TestEnv
import qualified Data.Aeson as A
import Test.Hspec

spec :: Spec
spec = before sysEnv $
  describe "ToJSON" $
    itRight "Request" . const $ do
      let req =
            SubmitOrder.Request
              testAmt
              [currencyPair|ADABTC|]
              [quotePerBaseBuy|0.00081037|]
              SubmitOrder.optsPostOnly
      liftIO $
        A.encode req
          `shouldBe` "{\"amount\":\"4.004004\",\"flags\":4096,\"price\":\"0.00081037\",\"symbol\":\"tADABTC\",\"type\":\"EXCHANGE LIMIT\"}"
