{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Maybe
import Data.Word
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@=?))
import Test.Tasty.Hedgehog (testProperty, HedgehogTestLimit(..))
import Hedgehog (MonadGen, property, forAll, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.IO as IO

import qualified Bitcoin.Keys as S

--------------------------------------------------------------------------------

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  Tasty.defaultMainWithIngredients
    [ Tasty.consoleTestReporter
    , Tasty.listingTests
    ] $ Tasty.localOption (HedgehogTestLimit (Just 2000))
      $ tt

tt :: TestTree
tt = testGroup "bitcoin-keys"
  [ testCase "Show Pub" $ do
      show pub1c @?= "Pub 024ed59d3f6548c41172ec26b9144f547185cb2a9f28fccb6e3ae53dc4f1827154"

  , testCase "Show Prv" $ do
      show prv1 @?= "Prv 76deed24fcdfb479656af6c5c4ab906090c5ee39da1c1ad1f1ad56dfce378a03"

  , testCase "Show Tweak" $ do
      show tweak1 @?= "Tweak 0ef561fb5f4e9e2319a5b75ed3be3c895e6ec272c0b5185dc414fb7e47c3a770"

  , testCase "parsePub: uncompressed vs compressed (even y)" $ do
      pub1c @=? pub1u
      S.pubCompressed pub1c @?= pub1c_raw
      S.pubCompressed pub1u @?= pub1c_raw
      S.pubUncompressed pub1c @?= pub1u_raw
      S.pubUncompressed pub1u @?= pub1u_raw

  , testCase "parsePub: uncompressed vs compressed (odd y)" $ do
      pub2c @=? pub2u
      S.pubCompressed pub2c @?= pub2c_raw
      S.pubCompressed pub2u @?= pub2c_raw
      S.pubUncompressed pub2c @?= pub2u_raw
      S.pubUncompressed pub2u @?= pub2u_raw

  , testCase "Ord Pub" $ do
      compare pub1c pub2c @?= LT
      compare pub2c pub1c @?= GT
      compare pub1c pub1c @?= EQ
      compare pub2c pub2c @?= EQ

  , testCase "Ord Prv" $ do
      compare prv1 prv2 @?= LT
      compare prv2 prv1 @?= GT
      compare prv1 prv1 @?= EQ
      compare prv2 prv2 @?= EQ

  , testCase "prvToPub prv1" $ do
      S.prvToPub prv1 @?= pub1c

  , testCase "prvToPub prv2" $ do
      S.prvToPub prv2 @?= pub2c

  , testProperty "parsePrv . prvRaw" $ property $ do
      prv <- forAll genPrv
      Just prv === S.parsePrv (S.prvRaw prv)

  , testProperty "parsePub . pubCompressed" $ property $ do
      pub <- forAll genPub
      Just pub === S.parsePub (S.pubCompressed pub)

  , testProperty "parsePub . pubUncompressed" $ property $ do
      pub <- forAll genPub
      Just pub === S.parsePub (S.pubUncompressed pub)

  , testProperty "parsePrv: invalid" $ property $ do
      raw <- forAll $ Gen.bytes (Range.constant 0 200)
      case B.length raw of
         32 -> pure ()
         _  -> Nothing === S.parsePrv raw

  , testProperty "parsePub: invalid" $ property $ do
      raw <- forAll $ Gen.bytes (Range.constant 0 200)
      case B.length raw of
         33 | B.head raw `elem` [0x02, 0x03] -> pure ()
         65 | B.head raw == 0x04 -> pure ()
         _  -> Nothing === S.parsePub raw

  , testProperty "parseTweak" $ property $ do
      raw <- forAll $ Gen.bytes (Range.constant 0 200)
      let yt = S.parsePrv raw
      case B.length raw of
         32 -> True === isJust yt
         _  -> Nothing === yt

  , testProperty "addPrvTweak vs addPubTweak (prop)" $ property $ do
      tweak <- forAll genTweak
      prv <- forAll genPrv
      let pub = S.prvToPub prv
          yprv' = S.prvAddTweak tweak prv
          ypub' = S.pubAddTweak tweak pub
      ypub' === fmap S.prvToPub yprv'

  , testCase "addPrvTweak vs addPubTweak (prv1)" $ do
       let Just pub' = S.prvToPub <$> S.prvAddTweak tweak1 prv1
       Just pub' @?= S.pubAddTweak tweak1 pub1c

  , testCase "addPrvTweak vs addPubTweak (prv2)" $ do
       let Just pub' = S.prvToPub <$> S.prvAddTweak tweak1 prv2
       Just pub' @?= S.pubAddTweak tweak1 pub2c

  , testCase "addPrvTweak prv1" $ do
      let Just x = S.parsePrv $ fromBase16
            "85d44f205c2e529c7f10ae249869cce9ef34b0ac9ad1332fb5c2525e15fb3173"
      S.prvAddTweak tweak1 prv1 @?= Just x

  , testCase "addPrvTweak prv2" $ do
      let Just x = S.parsePrv $ fromBase16
            "cb186f2bc6a2ace5580fb1fda1861ea2757435aa1e66a812d7decb7d368eddee"
      S.prvAddTweak tweak1 prv2 @?= Just x

  , testCase "addPubTweak pub1" $ do
      let Just x = S.parsePub $ fromBase16
            "038089e37ec58d5ace95bc84ee11d6c366d1b464ba703c2f8ec189bc2bce76f1f3"
      S.pubAddTweak tweak1 pub1c @?= Just x

  , testCase "addPubTweak pub2" $ do
      let Just x = S.parsePub $ fromBase16
            "025c8fd62030ba8ee8ede5bf053613a31a4850c9489eb62bec7cfb3efaa1e07b43"
      S.pubAddTweak tweak1 pub2c @?= Just x
  ]

--------------------------------------------------------------------------------

genTweak :: MonadGen m => m S.Tweak
genTweak = do
  b <- Gen.bytes (Range.singleton 32)
  let Just k = S.parseTweak b
  pure k

genPrv :: MonadGen m => m S.Prv
genPrv = do
  b <- Gen.bytes (Range.singleton 32)
  let Just k = S.parsePrv b
  pure k

genPub :: MonadGen m => m S.Pub
genPub = go 10000 where
  go 0 = error "genPub: too many attempts"
  go n = do
    h <- Gen.element [2, 3 :: Word8]
    b <- Gen.bytes (Range.singleton 32)
    case S.parsePub (B.cons h b) of
      Just k -> pure k
      Nothing -> go (n - 1)

--------------------------------------------------------------------------------
-- BIP39 mnemonic: abandon abandon arm
--
-- BIP39 seed: 2f7b8b6eeeb7201d88ed45dcd4706ccd76e5feb61f1961990869581781f8944ae2bebff36156c6965054add29f2cf9ee8b7b488179bfba24773ce2b06ec0ce04
--
-- BIP32 Derivation path: m/44'/0'/0'/0/0
--
-- Private key WIF: L1CnCq2yTvfzngQTF4Fnh5daYUwAuysrqU5dNwZFcv33Cwu4Gf2F

prv1 :: S.Prv
prv1 = maybe (error "prv1") id (S.parsePrv prv1_raw)

prv1_raw :: B.ByteString
prv1_raw = fromBase16
  "76deed24fcdfb479656af6c5c4ab906090c5ee39da1c1ad1f1ad56dfce378a03"

-- | Compressed version of `pub1u`
pub1c :: S.Pub
pub1c = maybe (error "pub1c") id (S.parsePub pub1c_raw)

pub1c_raw :: B.ByteString
pub1c_raw = fromBase16
  "024ed59d3f6548c41172ec26b9144f547185cb2a9f28fccb6e3ae53dc4f1827154"

-- | Uncompressed version of `pub1c`
pub1u :: S.Pub
pub1u = maybe (error "pub1u") id (S.parsePub pub1u_raw)

pub1u_raw :: B.ByteString
pub1u_raw = fromBase16
  "044ed59d3f6548c41172ec26b9144f547185cb2a9f28fccb6e3ae53dc4f1827154\
    \ba3a12d8b383bfff1dacd6930285f4a51a01ee64e102652d5bc137c49b97815e"

--------------------------------------------------------------------------------
-- BIP39 mnemonic: abandon abandon arm
--
-- BIP39 seed: 2f7b8b6eeeb7201d88ed45dcd4706ccd76e5feb61f1961990869581781f8944ae2bebff36156c6965054add29f2cf9ee8b7b488179bfba24773ce2b06ec0ce04
--
-- BIP32 Derivation path: m/84'/0'/0'/0/0
--
-- Private key WIF: L3XRaAfqnZLCcUpcCi4g1GqMLTKvr9nbvCYBmrs1TAjx4acpZ2eY

prv2 :: S.Prv
prv2 = maybe (error "prv2") id (S.parsePrv prv2_raw)

prv2_raw :: B.ByteString
prv2_raw = fromBase16
  "bc230d3067540ec23e69fa9ecdc7e219170573375db18fb513c9cffeeecb367e"

-- | Compressed version of `pub2u`
pub2c :: S.Pub
pub2c = maybe (error "pub2c") id (S.parsePub pub2c_raw)

pub2c_raw :: B.ByteString
pub2c_raw = fromBase16
  "0315bd146d8b45383ad431a8204717dc9f46a7a46c1f44eeaab02d49f4511ccff0"

-- | Uncompressed version of `pub2c`
pub2u :: S.Pub
pub2u = maybe (error "pub2u") id (S.parsePub pub2u_raw)

pub2u_raw :: B.ByteString
pub2u_raw = fromBase16
  "0415bd146d8b45383ad431a8204717dc9f46a7a46c1f44eeaab02d49f4511ccff0\
    \7d7817de34c0a281b45691eec966e95d936554b2320462214cd2a17aebcf460f"

--------------------------------------------------------------------------------
-- Randomly generated

tweak1 :: S.Tweak
tweak1 = maybe (error "tweak1") id (S.parseTweak tweak1_raw)

tweak1_raw :: B.ByteString
tweak1_raw = fromBase16
  "0ef561fb5f4e9e2319a5b75ed3be3c895e6ec272c0b5185dc414fb7e47c3a770"

--------------------------------------------------------------------------------

fromBase16 :: B.ByteString -> B.ByteString
fromBase16 a = case B16.decode a of
  (b, "") -> b
  _ -> error ("Invalid base16 string: " <> show a)

