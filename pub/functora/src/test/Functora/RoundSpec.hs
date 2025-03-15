module Functora.RoundSpec (spec) where

import Data.Fixed (E12, Fixed)
import Functora.Prelude (throw, throwString, try)
import Functora.Round (dpRound, sdRound)
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))
import Test.DocTest (doctest)
import Test.Hspec
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as HU (testCase, (@?=))
import qualified Test.Tasty.QuickCheck as QC
import Prelude

spec :: Spec
spec = do
  it "tasty" $ do
    res <- try $ defaultMain tests
    case res of
      Right () -> throwString @String "unexpected tasty result"
      Left ExitSuccess -> pure ()
      Left e -> throw e
  it "doctest" $
    doctest
      [ "-isrc",
        "src/round/Functora/Round.hs"
      ]

  mkRoundSpec @(Fixed E12) "dpRound/Fixed" dpRound dpRoundTestData
  mkRoundSpec @Rational "dpRound/Rational" dpRound dpRoundTestData
  mkRoundSpec @Double "dpRound/Double" dpRound dpRoundTestData

  mkRoundSpec @(Fixed E12) "sdRound/Fixed" sdRound sdRoundTestData
  mkRoundSpec @Rational "sdRound/Rational" sdRound sdRoundTestData
  mkRoundSpec @Double "sdRound/Double" sdRound sdRoundTestData

--
-- Tasty
--

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ units,
      properties
    ]

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ qcProps
    ]

units :: TestTree
units =
  testGroup
    "Units"
    [ roundUnits
    ]

roundUnits :: TestTree
roundUnits =
  testGroup
    "Rounding ..."
    [ testGroup
        "Rounding to 2 decimal places"
        [ HU.testCase "123456789.0 => no change" $
            dpRound 2 (123456789.0 :: Rational) @?= 123456789.0,
          HU.testCase "1234.56789 => 1234.57" $
            dpRound 2 (1234.56789 :: Rational) @?= 1234.57,
          HU.testCase "123.456789 => 123.46" $
            dpRound 2 (123.456789 :: Rational) @?= 123.46,
          HU.testCase "12.3456789 => 12.35" $
            dpRound 2 (12.3456789 :: Rational) @?= 12.35,
          HU.testCase "1.23456789 => 1.23" $
            dpRound 2 (1.23456789 :: Rational) @?= 1.23,
          HU.testCase "0.123456789 => 0.12" $
            dpRound 2 (0.123456789 :: Rational) @?= 0.12,
          HU.testCase "0.0123456789 => 0.01" $
            dpRound 2 (0.0123456789 :: Rational) @?= 0.01,
          HU.testCase "0.0000123456789 => 0.0" $
            dpRound 2 (0.0000123456789 :: Rational) @?= 0.0
        ],
      testGroup
        "Rounding 4 significant digits"
        [ HU.testCase "123456789.0 => 123500000.0" $
            sdRound 4 (123456789.0 :: Rational) @?= 123500000.0,
          HU.testCase "1234.56789 => 1235.0" $
            sdRound 4 (1234.56789 :: Rational) @?= 1235.0,
          HU.testCase "123.456789 => 123.5" $
            sdRound 4 (123.456789 :: Rational) @?= 123.5,
          HU.testCase "12.3456789 => 12.35" $
            sdRound 4 (12.3456789 :: Rational) @?= 12.35,
          HU.testCase "1.23456789 => 1.235" $
            sdRound 4 (1.23456789 :: Rational) @?= 1.235,
          HU.testCase "0.123456789 => 0.1235" $
            sdRound 4 (0.123456789 :: Rational) @?= 0.1235,
          HU.testCase "0.0123456789 => 0.01235" $
            sdRound 4 (0.0123456789 :: Rational) @?= 0.01235,
          HU.testCase "0.0000123456789 => 0.00001235" $
            sdRound 4 (0.0000123456789 :: Rational) @?= 0.00001235
        ]
    ]

qcProps :: TestTree
qcProps =
  testGroup
    "(checked by QuickCheck)"
    [ QC.testProperty "Rounding is idempotent" dpIdempotent
    ]

dpIdempotent :: Integer -> Rational -> Bool
dpIdempotent dp x =
  let y = dpRound dp x in dpRound dp y == y

mkRoundSpec ::
  (Show a, RealFrac a) => String -> (b -> a -> a) -> [(b, a, a)] -> Spec
mkRoundSpec label f =
  it label . mapM_ (\(x, prev, next) -> f x prev `shouldBe` next)

-- | Every element is a tuple (decimalPlaces, beforeRound, afterRound)
dpRoundTestData :: (RealFrac a) => [(Integer, a, a)]
dpRoundTestData =
  [ (2, 123456789.0, 123456789.0),
    (2, 1234.56789, 1234.57),
    (2, 123.456789, 123.46),
    (2, 12.3456789, 12.35),
    (2, 1.23456789, 1.23),
    (2, 0.123456789, 0.12),
    (2, 0.0123456789, 0.01),
    (2, 0.00123456789, 0.00),
    (3, 123456789.0, 123456789.0),
    (3, 1234.56789, 1234.568),
    (3, 123.456789, 123.457),
    (3, 12.3456789, 12.346),
    (3, 1.23456789, 1.235),
    (3, 0.123456789, 0.123),
    (3, 0.0123456789, 0.012),
    (3, 0.00123456789, 0.001),
    (3, 0.000123456789, 0.000),
    (4, 123456789.0, 123456789.0),
    (4, 1234.56789, 1234.5679),
    (4, 123.456789, 123.4568),
    (4, 12.3456789, 12.3457),
    (4, 1.23456789, 1.2346),
    (4, 0.123456789, 0.1235),
    (4, 0.0123456789, 0.0123),
    (4, 0.00123456789, 0.0012),
    (4, 0.000123456789, 0.0001),
    (4, 0.0000123456789, 0.0000),
    (5, 123456789.0, 123456789.0),
    (5, 1234.56789, 1234.56789),
    (5, 123.456789, 123.45679),
    (5, 12.3456789, 12.34568),
    (5, 1.23456789, 1.23457),
    (5, 0.123456789, 0.12346),
    (5, 0.0123456789, 0.01235),
    (5, 0.00123456789, 0.00123),
    (5, 0.000123456789, 0.00012),
    (5, 0.0000123456789, 0.00001),
    (5, 0.00000123456789, 0.0),
    (6, 123456789.0, 123456789.0),
    (6, 1234.56789, 1234.56789),
    (6, 123.456789, 123.456789),
    (6, 12.3456789, 12.345679),
    (6, 1.23456789, 1.234568),
    (6, 0.123456789, 0.123457),
    (6, 0.0123456789, 0.012346),
    (6, 0.00123456789, 0.001235)
  ]

-- | Every element is a tuple (significantDigits, beforeRound, afterRound)
sdRoundTestData :: (RealFrac a) => [(Natural, a, a)]
sdRoundTestData =
  [ (4, 123456789.0, 123500000.0),
    (4, 1234.56789, 1235.0),
    (4, 123.456789, 123.5),
    (4, 12.3456789, 12.35),
    (4, 1.23456789, 1.235),
    (4, 0.123456789, 0.1235),
    (4, 0.0123456789, 0.01235),
    (4, 0.00123456789, 0.001235),
    (4, 0.000123456789, 0.0001235),
    (5, 123456789.0, 123460000.0),
    (5, 1234.56789, 1234.6),
    (5, 123.456789, 123.46),
    (5, 12.3456789, 12.346),
    (5, 1.23456789, 1.2346),
    (5, 0.123456789, 0.12346),
    (5, 0.0123456789, 0.012346),
    (5, 0.00123456789, 0.0012346),
    (5, 0.000123456789, 0.00012346),
    (6, 123456789.0, 123457000.0),
    (6, 1234.56789, 1234.57),
    (6, 123.456789, 123.457),
    (6, 12.3456789, 12.3457),
    (6, 1.23456789, 1.23457),
    (6, 0.123456789, 0.123457),
    (6, 0.0123456789, 0.0123457),
    (6, 0.00123456789, 0.00123457),
    (6, 0.000123456789, 0.000123457),
    (7, 123456789.0, 123456800.0),
    (7, 1234.56789, 1234.568),
    (7, 123.456789, 123.4568),
    (7, 12.3456789, 12.34568),
    (7, 1.23456789, 1.234568),
    (7, 0.123456789, 0.1234568)
  ]
