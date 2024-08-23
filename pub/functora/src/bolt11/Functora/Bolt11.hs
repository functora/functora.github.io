{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Functora.Bolt11
  ( Word5,
    Hex (..),
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

import qualified Bitcoin.Address as Btc
import qualified Bitcoin.Address.Hash as Btc
import qualified Bitcoin.Address.SegWit as SegWit
import qualified Bitcoin.Address.Settings as Btc
import Codec.Binary.Bech32 (Word5)
import qualified Codec.Binary.Bech32.Internal as Bech32
import Control.Applicative
import qualified Data.Attoparsec.Text as Atto
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Functora.Prelude hiding (error)
import Prelude (Show (..), error)

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
  | AdditionalMetadata [Word5]
  | PayeePubkey Hex
  | DescriptionHash Hex
  | Expiry Int
  | MinFinalCltvExpiry Int
  | OnchainFallback Btc.Address
  | ExtraRouteInfo
  | FeatureBits [Word5]
  | UnknownTag Int [Word5]
  | UnparsedTag Int [Word5] String
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

networkSettings :: Network -> Btc.Settings
networkSettings = \case
  BitcoinMainnet -> Btc.btc
  BitcoinTestnet -> Btc.btcTestnet
  BitcoinRegtest ->
    Btc.btcTestnet
      { Btc.settings_prefixSegWit =
          fromMaybe (error "Bad prefixSegWit") $ Btc.prefixSegWit "bcrt"
      }
  BitcoinSignet ->
    Btc.btcTestnet
      { Btc.settings_prefixSegWit =
          fromMaybe (error "Bad prefixSegWit") $ Btc.prefixSegWit "tbs"
      }

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

parseNetwork :: Atto.Parser Network
parseNetwork =
  (Atto.string "bcrt" *> pure BitcoinRegtest)
    <|> (Atto.string "bc" *> pure BitcoinMainnet)
    <|> (Atto.string "tbs" *> pure BitcoinSignet)
    <|> (Atto.string "tb" *> pure BitcoinTestnet)

parseMultiplier :: Atto.Parser Multiplier
parseMultiplier = do
  c <- Atto.satisfy (`elem` ("munp" :: String))
  case c of
    'm' -> pure Milli
    'u' -> pure Micro
    'n' -> pure Nano
    'p' -> pure Pico
    _ -> fail "unhandled case in parseMultiplier"

parseHrpAmount :: Atto.Parser Bolt11HrpAmt
parseHrpAmount = do
  amt <- Atto.decimal
  mul <- parseMultiplier
  pure $ Bolt11HrpAmt amt mul

parseHrp :: Atto.Parser Bolt11Hrp
parseHrp = do
  _ <- Atto.char 'l'
  _ <- Atto.char 'n'
  net <- parseNetwork
  amt <- optional parseHrpAmount
  pure (Bolt11Hrp net amt)

w5bs :: [Word5] -> Either String ByteString
w5bs =
  maybe (Left "Non-Base256 bits") (Right . BS.pack)
    . Bech32.toBase256

w5hex :: [Word5] -> Either String Hex
w5hex =
  second Hex . w5bs

w5txt :: [Word5] -> Either String Text
w5txt =
  first displayException . decodeUtf8' <=< w5bs

w5int :: [Word5] -> Int
w5int bytes = foldl' decodeInt 0 (zip [0 ..] (take 7 (reverse bytes)))
  where
    decodeInt !n (i, byte) =
      n .|. fromEnum byte `shiftL` (i * 5)

w5addr :: Network -> [Word5] -> Either String Btc.Address
w5addr _ [] = Left "Empty Onchain Fallback Address"
w5addr net (v0 : rest) =
  case fromEnum v0 of
    -- SegWit
    vsn | vsn <= 16 -> do
      sv0 <- first displayException $ tryFrom @Int @Word8 vsn
      sv1 <-
        maybe
          (Left $ "Bad SegWit verion " <> inspect sv0)
          Right
          $ SegWit.version sv0
      raw <- w5bs rest
      res <- maybe (Left "Bad SegWit program") Right $ SegWit.program sv1 raw
      pure $ Btc.SegWit (Btc.settings_prefixSegWit cfg) res
    -- P2PKH
    17 -> do
      raw <- w5bs rest
      res <- maybe (Left "Bad PubHash160") Right $ Btc.parsePubHash160 raw
      pure $ Btc.P2PKH (Btc.settings_prefixP2PKH cfg) res
    -- P2SH
    18 -> do
      raw <- w5bs rest
      res <- maybe (Left "Bad ScriptHash160") Right $ Btc.parseScriptHash160 raw
      pure $ Btc.P2SH (Btc.settings_prefixP2SH cfg) res
    vsn -> do
      Left $ "Bad Onchain Fallback verion " <> inspect vsn
  where
    cfg = networkSettings net

parseTag :: Network -> [Word5] -> (Maybe Tag, [Word5])
parseTag _ [] = (Nothing, [])
parseTag _ ws@[_] = (Nothing, ws)
parseTag _ ws@[_, _] = (Nothing, ws) -- appease the compiler warning gods
parseTag _ ws
  | length ws < 8 = (Nothing, ws)
parseTag net ws@(t0 : d1 : d2 : rest)
  | length rest < 7 = (Nothing, ws)
  | otherwise = (Just tag, leftovers)
  where
    typ = fromEnum t0
    len = w5int [d1, d2]
    (dat, leftovers) = splitAt len rest
    hex = w5hex dat
    txt = w5txt dat
    mkt :: (a -> Tag) -> Either String a -> Tag
    mkt con = either (UnparsedTag typ dat) con
    tag =
      case typ of
        1 -> mkt PaymentHash hex
        16 -> mkt PaymentSecret hex
        13 -> mkt Description txt
        27 -> AdditionalMetadata dat
        19 -> mkt PayeePubkey hex
        23 -> mkt DescriptionHash hex
        6 -> Expiry $ w5int dat
        24 -> MinFinalCltvExpiry $ w5int dat
        9 -> mkt OnchainFallback $ w5addr net dat
        3 -> ExtraRouteInfo
        5 -> FeatureBits dat
        n -> UnknownTag n dat

data MSig
  = Sig [Word5]
  | Unk [Word5]
  deriving stock (Eq, Ord, Show, Generic)

parseTags :: Network -> [Word5] -> ([Tag], MSig)
parseTags net ws
  | length ws == 104 = ([], Sig ws)
  | otherwise =
      let (mtag, rest) = parseTag net ws
       in maybe
            ([], Unk rest)
            (\tag -> first (tag :) (parseTags net rest))
            mtag

decodeBolt11 :: Text -> Either String Bolt11
decodeBolt11 raw = do
  (rawHrp, rawDp) <- first show $ Bech32.decodeLenient raw
  hrp <- Atto.parseOnly parseHrp $ Bech32.humanReadablePartToText rawHrp
  let (ts, rest) = splitAt 7 $ Bech32.dataPartToWords rawDp
  let (tags, leftover) = parseTags (bolt11HrpNet hrp) rest
  (rawSig, recFlag) <- case leftover of
    Sig ws -> Right $ splitAt 103 ws
    Unk left -> Left ("corrupt, leftover: " <> show left)
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
