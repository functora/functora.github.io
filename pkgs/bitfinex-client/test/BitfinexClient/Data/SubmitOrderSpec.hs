{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.SubmitOrderSpec
  ( spec,
  )
where

import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import BitfinexClient.TestEnv
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
          `shouldBe` "{\"amount\":\"2.002002\",\"flags\":4096,\"price\":\"0.00081037\",\"symbol\":\"tADABTC\",\"type\":\"EXCHANGE LIMIT\"}"
