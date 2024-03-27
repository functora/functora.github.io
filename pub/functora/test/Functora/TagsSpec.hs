module Functora.TagsSpec
  ( spec,
  )
where

import Functora.Prelude
import Functora.Tags
import Functora.Tags.TestFgpt ()
import Functora.Tags.TestSing
import Test.Hspec

newMoney :: forall tags. Tagged (tags |+| 'Money) Rational
newMoney = Tagged $ 4 % 5

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
      (newMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      (newMoney @(Tags 'Net |+| "BTC" |+| 'Gain))
  it "UnTagFamily" $ do
    shouldBe
      (newMoney @(Tags "BTC" |+| 'Net |+| 'Gain |-| "BTC"))
      (newMoney @(Tags 'Net |+| 'Gain))
    shouldBe
      (newMoney @(Tags 'Net |+| 'Gain |-| 'Net |-| 'Gain))
      (newMoney @NoTags)
  it "Demote/Symbol" $ do
    getSymbolTag (newMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` "BTC"
  it "Demote/GainOrLose" $ do
    getGainOrLoseTag (newMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` Gain
  it "inspect" $ do
    inspect @Text (newMoney @NoTags)
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (newMoney @(NoTags |+| "BTC" |+| 'Net))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (newMoney @(Tags "BTC" |+| 'Net |+| 'Lose |+| 'Merchant))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (newMoney @(Tags "BTC" |+| 'Net |+| 'Lose))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (newMoney @(Tags "BTC"))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (newMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
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
