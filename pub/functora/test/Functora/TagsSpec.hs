module Functora.TagsSpec
  ( spec,
  )
where

import Functora.Prelude
import Functora.Tags
import Functora.Tags.TestFgpt ()
import Functora.Tags.TestSing
import Test.Hspec

mkMoney :: forall tags. Tagged (tags |+| 'Money) Rational
mkMoney = Tagged $ 4 % 5

getSymbolTag ::
  forall (tag :: Symbol) tags rep.
  ( GetTag tag tags
  ) =>
  Tagged tags rep ->
  Demote Symbol
getSymbolTag _ =
  demote @tag

getGainOrLoseTag ::
  forall (tag :: GainOrLose) tags rep.
  ( GetTag tag tags
  ) =>
  Tagged tags rep ->
  GainOrLose
getGainOrLoseTag _ =
  demote @tag

--
-- TODO : Typechecker tests using GHC.runGhc
-- maybe combined with TH.runQ, need more research.
-- Sounds like a very difficult but also a very fun thing to do.
--
spec :: Spec
spec = do
  it "Tags/Eq" $ do
    shouldBe
      (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      (mkMoney @(Tags 'Net |+| "BTC" |+| 'Gain))
  it "UnTagFamily" $ do
    shouldBe
      (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain |-| "BTC"))
      (mkMoney @(Tags 'Net |+| 'Gain))
    shouldBe
      (mkMoney @(Tags 'Net |+| 'Gain |-| 'Net |-| 'Gain))
      (mkMoney @NoTags)
  it "Demote/Symbol" $ do
    getSymbolTag (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` "BTC"
  it "Demote/GainOrLose" $ do
    getGainOrLoseTag (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` Gain
  it "inspect" $ do
    inspect @Text (mkMoney @NoTags)
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(NoTags |+| "BTC" |+| 'Net))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC" |+| 'Net |+| 'Lose |+| 'Merchant))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC" |+| 'Net |+| 'Lose))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC"))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` "Tagged (4 % 5)"
  it "inspectTags" $ do
    inspectTags @NoTags @Text
      `shouldBe` "'[] *"
    inspectTags @(NoTags |+| "BTC" |+| 'Net) @Text
      `shouldBe` "': * (Sing NetOrGross 'Net) (': * (Sing Symbol \"BTC\") ('[] *))"
    inspectTags @(Tags "BTC" |+| 'Net |+| 'Gain) @Text
      `shouldBe` "': * (Sing NetOrGross 'Net) (': * (Sing GainOrLose 'Gain) (': * (Sing Symbol \"BTC\") ('[] *)))"
    inspectTags @(Tags "BTC" |+| 'Net |+| 'Gain) @Text
      `shouldBe` inspectTags @(NoTags |+| "BTC" |+| 'Net |+| 'Gain)