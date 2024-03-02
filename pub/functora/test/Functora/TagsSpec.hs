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
  it "Demote/Symbol" $ do
    getSymbolTag (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` "BTC"
  it "Demote/GainOrLose" $ do
    getGainOrLoseTag (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` Gain
  it "inspect" $ do
    inspect @Text (mkMoney @(Tags "BTC" |+| 'Net |+| 'Lose |+| 'Merchant))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC" |+| 'Net |+| 'Lose))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC"))
      `shouldBe` "Tagged (4 % 5)"
    inspect @Text (mkMoney @(Tags "BTC" |+| 'Net |+| 'Gain))
      `shouldBe` "Tagged (4 % 5)"
  it "inspectTags" $ do
    inspectTags @(Tags "BTC" |+| 'Net |+| 'Gain) @Text
      `shouldBe` "': * (SNetOrGross 'Net) (': * (SGainOrLose 'Gain) (': * (SSymbol \"BTC\") ('[] *)))"
