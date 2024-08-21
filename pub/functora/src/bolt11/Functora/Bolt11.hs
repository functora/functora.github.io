{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Functora.Bolt11
  ( Bolt11 (..),
    decodeBolt11,
    Hex (..),
    Multiplier (..),
    Network (..),
    Tag (..),
    Bolt11Hrp (..),
    Bolt11HrpAmt (..),
    isPaymentHash,
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
  deriving newtype (Eq, Ord)
  deriving stock (Data, Generic)

instance Show Hex where
  show (Hex bs) = BL.unpack (BS.toLazyByteString (BS.byteStringHex bs))

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
  | OnchainFallback Hex -- TODO: address type
  | ExtraRouteInfo
  | FeatureBits [Word5]
  deriving stock (Eq, Ord, Show, Generic)

isPaymentHash :: Tag -> Bool
isPaymentHash PaymentHash {} = True
isPaymentHash _ = False

data Multiplier = Milli | Micro | Nano | Pico
  deriving stock (Eq, Ord, Show, Data, Generic)

data Network
  = BitcoinMainnet
  | BitcoinTestnet
  | BitcoinRegtest
  | BitcoinSignet
  deriving stock (Eq, Ord, Show, Data, Generic)

data Bolt11HrpAmt = Bolt11HrpAmt
  { bolt11HrpAmtNum :: Integer,
    bolt11HrpAmtMul :: Multiplier
  }
  deriving stock (Eq, Ord, Data, Generic)

instance Show Bolt11HrpAmt where
  show (Bolt11HrpAmt amt mul) =
    if sat > 1_000_000
      then inspectBtcNum btc <> " BTC"
      else
        if (round sat) % 1 == sat
          then inspectBtcNum sat <> " Satoshi"
          else inspectBtcNum msat <> " Millisatoshi"
    where
      btc :: Rational
      btc = (amt % 1) * multiplierRatio mul
      sat :: Rational
      sat = btc * 1_0000_0000
      msat :: Rational
      msat = sat * 1000

data Bolt11Hrp = Bolt11Hrp
  { bolt11HrpNet :: Network,
    bolt11HrpAmt :: Maybe Bolt11HrpAmt
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Bolt11 = Bolt11
  { bolt11Hrp :: Bolt11Hrp,
    bolt11Timestamp :: Int, -- posix
    bolt11Tags :: [Tag], -- posix
    bolt11Signature :: Hex
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

hrpParser :: Parser Bolt11Hrp
hrpParser = do
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

tagParser :: [Word5] -> (Maybe Tag, [Word5])
tagParser [] = (Nothing, [])
tagParser ws@[_] = (Nothing, ws)
tagParser ws@[_, _] = (Nothing, ws) -- appease the compiler warning gods
tagParser ws
  | length ws < 8 = (Nothing, ws)
tagParser ws@(typ : d1 : d2 : rest)
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
        9 -> OnchainFallback datBs
        3 -> ExtraRouteInfo
        5 -> FeatureBits dat
        n -> error ("unhandled typ " ++ show n)

data MSig
  = Sig [Word5]
  | Unk [Word5]
  deriving stock (Eq, Ord, Show, Generic)

tagsParser :: [Word5] -> ([Tag], MSig)
tagsParser ws
  | length ws == 104 = ([], Sig ws)
  | otherwise =
      let (mtag, rest) = tagParser ws
       in maybe
            ([], Unk rest)
            (\tag -> first (tag :) (tagsParser rest))
            mtag

decodeBolt11 :: Text -> Either String Bolt11
decodeBolt11 txt = do
  (hrp, w5s) <- first show $ Bech32.decodeLenient txt
  let (timestampBits, rest) = splitAt 7 $ Bech32.dataPartToWords w5s
      timestamp = w5int timestampBits
      (tags, leftover) = tagsParser rest
  sig <- case leftover of
    Sig ws -> maybe (Left "corrupt") Right (Bech32.toBase256 ws)
    Unk left -> Left ("corrupt, leftover: " ++ show (Hex (w5bs left)))
  parsedHrp <- parseOnly hrpParser $ Bech32.humanReadablePartToText hrp
  Right (Bolt11 parsedHrp timestamp tags (Hex (BS.pack sig)))

multiplierRatio :: Multiplier -> Rational
multiplierRatio m =
  case m of
    Milli -> 1 % 1000
    Micro -> 1 % 1000000
    Nano -> 1 % 1000000000
    Pico -> 1 % 1000000000000

inspectBtcNum ::
  forall a b.
  ( From String a,
    From b Integer,
    Integral b
  ) =>
  Ratio b ->
  a
inspectBtcNum =
  inspectRatio
    RatioFormat
      { ratioFormatDoRounding = True,
        ratioFormatThousandsSeparator = mempty,
        ratioFormatDecimalPlacesAfterNonZero = Just 12, -- Pico
        ratioFormatDecimalPlacesTotalLimit = Just 12, -- Pico
        ratioFormatDecimalPlacesTotalLimitOverflow = DecimalPlacesOverflowExponent
      }
