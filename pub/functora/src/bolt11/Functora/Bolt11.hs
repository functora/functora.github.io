{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Functora.Bolt11
  ( Hex (..),
    inspectHex,
    Tag (..),
    isPaymentHash,
    Network (..),
    Multiplier (..),
    Bolt11HrpAmt (..),
    inspectBolt11HrpAmt,
    Bolt11Hrp (..),
    Bolt11Sig (..),
    Bolt11 (..),
    decodeBolt11,
  )
where

import Codec.Binary.Bech32 (Word5)
import qualified Codec.Binary.Bech32.Internal as Bech32
import Control.Applicative
import Data.Attoparsec.Text
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Functora.Prelude hiding (error)
import Prelude (Show (..), error, splitAt, take)

newtype Hex = Hex
  { unHex :: ByteString
  }
  deriving stock (Eq, Ord, Data, Generic)

instance Show Hex where
  show = inspectHex

inspectHex :: forall a. (From String a) => Hex -> a
inspectHex =
  from @String @a
    . BL.unpack
    . BS.toLazyByteString
    . BS.byteStringHex
    . unHex

instance IsString Hex where
  fromString =
    Hex
      . handler
      . B16.decode
      . T.encodeUtf8
      . T.pack
    where
#if MIN_VERSION_base16_bytestring(1,0,0)
      handler :: Either String ByteString -> ByteString
      handler =
        either error id
#else
      handler :: (ByteString, ByteString) -> ByteString
      handler = \case
        (success, "") -> success
        failure -> error $ "Non-Hex bytestring " <> show failure
#endif

data Tag
  = PaymentHash Hex
  | PaymentSecret Hex
  | Description Text
  | PayeePubkey Hex
  | DescriptionHash Hex
  | Expiry Int
  | MinFinalCltvExpiry Int
  | OnchainFallback -- TODO: address type
  | ExtraRouteInfo
  | FeatureBits [Word5]
  deriving stock (Eq, Ord, Show, Generic)

isPaymentHash :: Tag -> Bool
isPaymentHash PaymentHash {} = True
isPaymentHash _ = False

data Network
  = BitcoinMainnet
  | BitcoinTestnet
  | BitcoinRegtest
  | BitcoinSignet
  deriving stock (Eq, Ord, Show, Data, Generic)

data Multiplier = Milli | Micro | Nano | Pico
  deriving stock (Eq, Ord, Show, Data, Generic)

data Bolt11HrpAmt = Bolt11HrpAmt
  { bolt11HrpAmtNum :: Integer,
    bolt11HrpAmtMul :: Multiplier
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

inspectBolt11HrpAmt ::
  ( From String a,
    Semigroup a,
    IsString a
  ) =>
  Bolt11HrpAmt ->
  a
inspectBolt11HrpAmt (Bolt11HrpAmt amt mul) =
  if sat > 1_000_000
    then inspectBolt11HrpAmt' btc <> " BTC"
    else
      if (round sat) % 1 == sat
        then inspectBolt11HrpAmt' sat <> " Satoshi"
        else inspectBolt11HrpAmt' msat <> " Millisatoshi"
  where
    btc :: Rational
    btc = (amt % 1) * multiplierRatio mul
    sat :: Rational
    sat = btc * 1_0000_0000
    msat :: Rational
    msat = sat * 1000

inspectBolt11HrpAmt' ::
  forall a b.
  ( From String a,
    From b Integer,
    Integral b
  ) =>
  Ratio b ->
  a
inspectBolt11HrpAmt' =
  inspectRatio
    RatioFormat
      { ratioFormatDoRounding = True,
        ratioFormatThousandsSeparator = mempty,
        ratioFormatDecimalPlacesAfterNonZero = Just 12, -- Pico
        ratioFormatDecimalPlacesTotalLimit = Just 12, -- Pico
        ratioFormatDecimalPlacesTotalLimitOverflow = DecimalPlacesOverflowExponent
      }

multiplierRatio :: Multiplier -> Rational
multiplierRatio m =
  case m of
    Milli -> 1 % 1000
    Micro -> 1 % 1000000
    Nano -> 1 % 1000000000
    Pico -> 1 % 1000000000000

data Bolt11Hrp = Bolt11Hrp
  { bolt11HrpNet :: Network,
    bolt11HrpAmt :: Maybe Bolt11HrpAmt
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Bolt11Sig = Bolt11Sig
  { bolt11SigVal :: Hex,
    bolt11SigRecoveryFlag :: Int
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Bolt11 = Bolt11
  { bolt11Hrp :: Bolt11Hrp,
    bolt11Timestamp :: Int, -- posix
    bolt11Tags :: [Tag], -- posix
    bolt11Signature :: Bolt11Sig
  }
  deriving stock (Eq, Ord, Show, Generic)

parseNetwork :: Parser Network
parseNetwork =
  (string "bcrt" *> pure BitcoinRegtest)
    <|> (string "bc" *> pure BitcoinMainnet)
    <|> (string "tbs" *> pure BitcoinSignet)
    <|> (string "tb" *> pure BitcoinTestnet)

parseMultiplier :: Parser Multiplier
parseMultiplier = do
  c <- satisfy (`elem` ("munp" :: String))
  case c of
    'm' -> pure Milli
    'u' -> pure Micro
    'n' -> pure Nano
    'p' -> pure Pico
    _ -> fail "unhandled case in parseMultiplier"

parseHrpAmount :: Parser Bolt11HrpAmt
parseHrpAmount = do
  amt <- decimal
  mul <- parseMultiplier
  pure $ Bolt11HrpAmt amt mul

parseHrp :: Parser Bolt11Hrp
parseHrp = do
  _ <- char 'l'
  _ <- char 'n'
  net <- parseNetwork
  amt <- optional parseHrpAmount
  pure (Bolt11Hrp net amt)

w5int :: [Word5] -> Int
w5int bytes = foldl' decodeInt 0 (zip [0 ..] (Prelude.take 7 (reverse bytes)))
  where
    decodeInt !n (i, byte) =
      n .|. fromEnum byte `shiftL` (i * 5)

w5bs :: [Word5] -> ByteString
w5bs = BS.pack . fromMaybe (error "what") . Bech32.toBase256

w5txt :: [Word5] -> Text
w5txt = decodeUtf8 . w5bs

parseTag :: [Word5] -> (Maybe Tag, [Word5])
parseTag [] = (Nothing, [])
parseTag ws@[_] = (Nothing, ws)
parseTag ws@[_, _] = (Nothing, ws) -- appease the compiler warning gods
parseTag ws
  | length ws < 8 = (Nothing, ws)
parseTag ws@(typ : d1 : d2 : rest)
  | length rest < 7 = (Nothing, ws)
  | otherwise = (Just tag, leftovers)
  where
    dataLen = w5int [d1, d2]
    (dat, leftovers) = Prelude.splitAt dataLen rest
    datBs = Hex (w5bs dat)
    tag =
      case fromEnum typ of
        1 -> PaymentHash datBs
        16 -> PaymentSecret datBs
        23 -> DescriptionHash datBs -- (w5bs dat)
        13 -> Description (w5txt dat)
        19 -> PayeePubkey datBs
        6 -> Expiry (w5int dat)
        24 -> MinFinalCltvExpiry (w5int dat)
        9 -> OnchainFallback
        3 -> ExtraRouteInfo
        5 -> FeatureBits dat
        n -> error ("unhandled typ " ++ show n)

data MSig
  = Sig [Word5]
  | Unk [Word5]
  deriving stock (Eq, Ord, Show, Generic)

parseTags :: [Word5] -> ([Tag], MSig)
parseTags ws
  | length ws == 104 = ([], Sig ws)
  | otherwise =
      let (mtag, rest) = parseTag ws
       in maybe
            ([], Unk rest)
            (\tag -> first (tag :) (parseTags rest))
            mtag

decodeBolt11 :: Text -> Either String Bolt11
decodeBolt11 raw = do
  (rawHrp, rawDp) <- first show $ Bech32.decodeLenient raw
  hrp <- parseOnly parseHrp $ Bech32.humanReadablePartToText rawHrp
  let (ts, rest) = splitAt 7 $ Bech32.dataPartToWords rawDp
  let (tags, leftover) = parseTags rest
  (rawSig, recFlag) <- case leftover of
    Sig ws -> Right $ splitAt 103 ws
    Unk left -> Left ("corrupt, leftover: " ++ show (Hex (w5bs left)))
  sig <-
    maybe (Left "corrupt") Right $ Bech32.toBase256 rawSig
  Right
    Bolt11
      { bolt11Hrp = hrp,
        bolt11Timestamp = w5int ts,
        bolt11Tags = tags,
        bolt11Signature =
          Bolt11Sig
            { bolt11SigVal = Hex $ BS.pack sig,
              bolt11SigRecoveryFlag = w5int recFlag
            }
      }
