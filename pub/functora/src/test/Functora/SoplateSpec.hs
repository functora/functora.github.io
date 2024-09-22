{-# LANGUAGE CPP #-}

#ifdef NOSOP

module Functora.SoplateSpec (spec) where
import Functora.Prelude
import Test.Hspec
spec :: Spec
spec = it "void" $ True `shouldBe` True

#else

module Functora.SoplateSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Functora.Prelude
import Functora.Soplate
import Test.Hspec
import Test.QuickCheck.Instances ()
import qualified Universum

newtype Buz = Buz
  { _unBuz :: Map ByteString Int
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  deriving stock (Eq, Ord, Show, Data, Generic)

data NoData = NoData
  { noDataExpr :: Expr,
    noDataByte :: ByteString,
    noDataSelf :: Maybe NoData
  }
  deriving stock (Eq, Ord, Show, Generic)

expr :: Expr
expr = Add (Sub (Lit 1) (Lit 2)) (Lit 3)

fun :: Expr -> Expr
fun = \case
  Add a b -> Mul a b
  Lit i -> Lit (i + 1)
  a -> a

spec :: Spec
spec = do
  it "inspect/data" $ do
    let x = Buz [("buf", 123), (BS.pack [106, 246, 171, 231, 231, 166, 121], 321)]
    Universum.show @Text x
      `shouldBe` "Buz {_unBuz = fromList [(\"buf\",123),(\"j\\246\\171\\231\\231\\166y\",321)]}"
    inspect @Text x
      `shouldBe` "Buz {_unBuz = fromList [(\"6af6abe7e7a679\",321),(\"buf\",123)]}"
  it "inspect/Textual" $ do
    inspect @Text ("HELLO" :: String) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: Text) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: TL.Text) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: ByteString) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: BL.ByteString) `shouldBe` "HELLO"
  it "soplate/fun" $
    over soplate fun expr
      `shouldBe` Mul (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "soplate/plus" $
    over soplate (+ (1 :: Int)) expr
      `shouldBe` Add (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "inspectSop/Textual" $ do
    inspectSop @Text ("HELLO" :: String) `shouldBe` "HELLO"
    inspectSop @Text ("HELLO" :: Text) `shouldBe` "HELLO"
    inspectSop @Text ("HELLO" :: TL.Text) `shouldBe` "HELLO"
    inspectSop @Text ("HELLO" :: ByteString) `shouldBe` "HELLO"
    inspectSop @Text ("HELLO" :: BL.ByteString) `shouldBe` "HELLO"
  it "inspectSop/NoData" $ do
    let nodata =
          NoData
            { noDataExpr = expr,
              noDataByte = "example",
              noDataSelf =
                Just
                  NoData
                    { noDataExpr = Lit 1,
                      noDataByte = BS.pack [106, 246, 171, 231, 231, 166, 121],
                      noDataSelf = Nothing
                    }
            }
    inspectSop @Text nodata
      `shouldBe` "NoData {noDataExpr = Add (Sub (Lit 1) (Lit 2)) (Lit 3), noDataByte = \"example\", noDataSelf = Just (NoData {noDataExpr = Lit 1, noDataByte = \"6af6abe7e7a679\", noDataSelf = Nothing})}"

#endif
