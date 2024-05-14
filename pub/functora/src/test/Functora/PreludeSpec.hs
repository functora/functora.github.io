module Functora.PreludeSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Functora.Prelude
import Test.Hspec
import Test.QuickCheck
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
  it "inspect/IsString" $ do
    inspect @Text ("HELLO" :: String) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: Text) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: TL.Text) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: ByteString) `shouldBe` "HELLO"
    inspect @Text ("HELLO" :: BL.ByteString) `shouldBe` "HELLO"
  it "uid" $ do
    htmlUid @Text nilUid `shouldBe` "uid-11111111111111111111111111111111"
    htmlUid @Text (addUid nilUid nilUid)
      `shouldBe` "uid-HXugtXVfQbdnt1bHDJcE9HU6kDMaPEJSQhN3moaHr6Hp"
  it "soplate/fun"
    $ over soplate fun expr
    `shouldBe` Mul (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "soplate/plus"
    $ over soplate (+ (1 :: Int)) expr
    `shouldBe` Add (Sub (Lit 2) (Lit 3)) (Lit 4)
  it "parseRatio/overflow"
    $ inspect @Text (parseRatio @Text @Word8 @(Either SomeException) "0.333")
    `shouldBe` "Left (ParseException {parseExceptionSource = \"0.333\", parseExceptionSourceType = Text, parseExceptionTargetType = Ratio Word8, parseExceptionFailure = \"Word8 numerator or denominator seems to be out of bounds, expected 333 % 1000 but got 77 % 232\"})"
  it "inspectRatio/samples" $ do
    let samples :: [(Rational, Text)] =
          [ (0, "0"),
            (1, "1"),
            (42, "42"),
            (0.3, "0.3"),
            (0.33, "0.33"),
            (0.333, "0.333"),
            (0.3333, "0.333"),
            (0.33333, "0.333"),
            (1 % 3, "0.333"),
            (0.6, "0.6"),
            (0.66, "0.66"),
            (0.666, "0.666"),
            (0.6666, "0.667"),
            (0.66666, "0.667"),
            (2 % 3, "0.667"),
            (1.3, "1.3"),
            (1.33, "1.33"),
            (1.333, "1.33"),
            (1.3333, "1.33"),
            (1.33333, "1.33"),
            (4 % 3, "1.33"),
            (1.6, "1.6"),
            (1.66, "1.66"),
            (1.666, "1.67"),
            (1.6666, "1.67"),
            (1.66666, "1.67"),
            (5 % 3, "1.67"),
            (10003.6, "10003.6"),
            (10003.66, "10003.66"),
            (10003.666, "10003.67"),
            (10003.6666, "10003.67"),
            (10003.66666, "10003.67"),
            (30011 % 3, "10003.67"),
            (10003.06, "10003.06"),
            (10003.006, "10003.01"),
            (10003.00000006, "10003"),
            (2 % 3000, "0.000667"),
            (2 % 30000, "0.0000667"),
            (2 % 300000, "6.67e-6"),
            (2 % 3000000, "6.67e-7"),
            (2 % 30000000, "6.67e-8"),
            (0.00000001030405, "1.03e-8")
          ]
    forM_ samples
      . uncurry
      $ \lhs rhs -> do
        inspectRatio defaultRatioFormat lhs `shouldBe` rhs
        when (lhs /= 0)
          $ inspectRatio defaultRatioFormat (-lhs)
          `shouldBe` "-" <> rhs
  inspectParseRatioSigned @Integer
  inspectParseRatioSigned @Int
  --
  -- NOTE : inspectRatio converts to Rational under the hood,
  -- so smaller Ratio types sometimes do not have partial isomorphism.
  --
  -- inspectParseRatioSigned @Int8
  inspectParseRatioSigned @Int16
  inspectParseRatioSigned @Int32
  inspectParseRatioSigned @Int64
  inspectParseRatioUnsigned @Natural
  inspectParseRatioUnsigned @Word
  --
  -- NOTE : inspectRatio converts to Rational under the hood,
  -- so smaller Ratio types sometimes do not have partial isomorphism.
  --
  -- inspectParseRatioUnsigned @Word8
  inspectParseRatioUnsigned @Word16
  inspectParseRatioUnsigned @Word32
  inspectParseRatioUnsigned @Word64
  it "inspectSop/IsString" $ do
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
  it "expBackOff" $ do
    let xs :: NonEmpty Natural = expBackOff @Natural <$> [0 .. 17]
    head xs `shouldBe` 1
    last xs `shouldBe` 131072
  it "expBackOffSecondsAfter"
    $ inspect @Text (expBackOffSecondsAfter @Natural 3 epoch)
    `shouldBe` "1970-01-01 00:00:08 UTC"

inspectParseRatioSigned ::
  forall a.
  ( Show a,
    Data a,
    Integral a,
    From a Integer
  ) =>
  Spec
inspectParseRatioSigned =
  inspectParseRatio @a
    ("inspectParseRatio/" <> inspectType @a)
    property

inspectParseRatioUnsigned ::
  forall a.
  ( Show a,
    Data a,
    Integral a,
    From a Integer
  ) =>
  Spec
inspectParseRatioUnsigned =
  inspectParseRatio @a
    ("inspectParseRatio/" <> inspectType @a)
    $ forAll uFractional

inspectParseRatio ::
  forall a.
  ( Show a,
    Data a,
    Integral a,
    From a Integer
  ) =>
  String ->
  (forall prop. (Testable prop) => (Ratio a -> prop) -> Property) ->
  Spec
inspectParseRatio title generator =
  it title . generator $ \rat -> do
    let txt = inspectRatio @Text @a defaultRatioFormat rat
    inspectRatio @Text @a defaultRatioFormat <$> parseRatio txt
      `shouldBe` Just txt

uFractional :: (Fractional a) => Gen a
uFractional =
  sized $ \n -> do
    denom <- choose (1, max 1 n)
    numer <- choose (0, n * denom)
    pure
      $ Universum.fromIntegral numer
      / Universum.fromIntegral denom
