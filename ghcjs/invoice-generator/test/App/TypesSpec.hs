module App.TypesSpec (spec) where

import App.Prelude
import App.Types
import App.Widgets.Templates
import qualified Data.Aeson as A
import qualified Data.Generics as Syb
import qualified Functora.Web as Web
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
  deriving stock (Eq, Ord, Show, Data, Generic)

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
  -- it "soplate"
  --   -- good?
  --   $ over soplate fun expr
  --   `shouldBe` Mul (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "serialization" $ do
    st0 <- newModel Web.defOpts Nothing =<< URI.mkURI "http://localhost"
    uri <- stUri st0
    st1 <- newModel Web.defOpts Nothing uri
    (st0 ^. #modelState . #stDoc . to uniqueToIdentity)
      `shouldBe` (st1 ^. #modelState . #stDoc . to uniqueToIdentity)
