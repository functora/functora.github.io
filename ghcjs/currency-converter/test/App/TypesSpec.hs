module App.TypesSpec (spec) where

import qualified App.Misc as Misc
import qualified App.Types as Types
import qualified Data.Aeson as A
import qualified Data.Generics as Syb
import Functora.Prelude
import qualified Optics.Generic as Ops
import qualified Optics.Setter as Ops
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import qualified Text.URI as URI

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving (Eq, Ord, Show, Data, Generic)

expr :: Expr
expr = Add (Sub (Lit 1) (Lit 2)) (Lit 3)

fun :: Expr -> Expr
fun = \case
  Add a b -> Mul a b
  Lit i -> Lit (i + 1)
  a -> a

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
  it "syb/everywhere"
    -- good?
    $ Syb.everywhere (Syb.mkT fun) expr
    `shouldBe` Mul (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "generic-lens/types"
    -- bad?
    $ over types fun expr
    `shouldBe` Mul (Sub (Lit 1) (Lit 2)) (Lit 3)
  it "optics-core/gplate"
    -- bad?
    $ Ops.over Ops.gplate fun expr
    `shouldBe` Add (Sub (Lit 1) (Lit 2)) (Lit 4)
  it "soplate"
    -- good?
    $ over soplate fun expr
    `shouldBe` Mul (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "serialization" $ do
    st0 <- Types.newModel =<< URI.mkURI "http://localhost"
    uri <- Misc.appUri st0
    st1 <- Types.newModel uri
    (st0 ^. #modelState . to Types.newIdentityState . #stDoc)
      `shouldBe` (st1 ^. #modelState . to Types.newIdentityState . #stDoc)
