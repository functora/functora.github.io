{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Type
  ( -- * Orders
    -- $orders
    OrderId (..),
    OrderClientId (..),
    OrderGroupId (..),
    Order (..),
    OrderFlag (..),
    OrderFlagAcc (..),
    unOrderFlag,
    unOrderFlagSet,
    OrderStatus (..),
    newOrderStatus,
    AffCode (..),

    -- * Trading
    -- $trading
    RebateRate (..),
    CurrencyCode (..),
    newCurrencyCode,
    CurrencyPair,
    currencyPairCon,
    currencyPairBase,
    currencyPairQuote,
    newCurrencyPair,
    CurrencyPairConf (..),
    Candle (..),
    CandleTimeFrame (..),
    Ticker (..),

    -- * Misc
    -- $misc
    RawResponse (..),
    PltStatus (..),
    Error (..),
    emptyReq,
    eradicateNull,
  )
where

import Bfx.Class.ToRequestParam
import Data.Aeson (withText)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import Functora.Cfg
import Functora.Money
import Functora.Prelude
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Web
import qualified Prelude

-- $orders
-- Order data received from Bitfinex
-- and types related to orders.

newtype OrderId = OrderId
  { unOrderId :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey,
      HasCodec,
      HasItemCodec
    )
  deriving stock
    ( Data,
      Generic
    )

newtype OrderClientId = OrderClientId
  { unOrderClientId :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey,
      HasCodec,
      HasItemCodec
    )
  deriving stock
    ( Data,
      Generic
    )

newtype OrderGroupId = OrderGroupId
  { unOrderGroupId :: Natural
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey,
      HasCodec,
      HasItemCodec
    )
  deriving stock
    ( Data,
      Generic
    )

data Order = Order
  { orderId :: OrderId,
    orderGroupId :: Maybe OrderGroupId,
    -- | Field might be auto-generated by Bitfinex in case where
    -- it was not provided through 'Bfx.Data.SubmitOrder.Options'.
    orderClientId :: Maybe OrderClientId,
    orderBaseAmount :: MoneyAmount,
    orderSymbol :: CurrencyPair,
    orderRate :: QuotePerBase,
    orderStatus :: OrderStatus,
    orderBuyOrSell :: BuyOrSell,
    orderLocalOrRemote :: LocalOrRemote
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data OrderFlag
  = Hidden
  | Close
  | ReduceOnly
  | PostOnly
  | Oco
  | NoVarRates
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic,
      Enum,
      Bounded
    )

newtype OrderFlagAcc
  = OrderFlagAcc Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      Num,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey
    )
  deriving stock
    ( Data,
      Generic
    )

unOrderFlag :: OrderFlag -> OrderFlagAcc
unOrderFlag =
  OrderFlagAcc . \case
    Hidden -> 64
    Close -> 512
    ReduceOnly -> 1024
    PostOnly -> 4096
    Oco -> 16384
    NoVarRates -> 524288

unOrderFlagSet :: Set OrderFlag -> OrderFlagAcc
unOrderFlagSet =
  foldr (\x acc -> acc + unOrderFlag x) $ OrderFlagAcc 0

data OrderStatus
  = Active
  | Executed
  | PartiallyFilled
  | InsufficientMargin
  | Cancelled
  | PostOnlyCancelled
  | RsnDust
  | RsnPause
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic,
      Enum,
      Bounded
    )
  deriving
    ( ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey
    )
    via GenericType OrderStatus
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericEnum OrderStatus

newOrderStatus ::
  Text ->
  Either
    (TryFromException Text OrderStatus)
    OrderStatus
newOrderStatus = \case
  "ACTIVE" -> Right Active
  x | "EXECUTED" `T.isPrefixOf` x -> Right Executed
  x | "PARTIALLY FILLED" `T.isPrefixOf` x -> Right PartiallyFilled
  x | "INSUFFICIENT MARGIN" `T.isPrefixOf` x -> Right InsufficientMargin
  x | "CANCELED" `T.isPrefixOf` x -> Right Cancelled
  --
  -- TODO : verify this, it's some corner case
  -- related to PartiallyFilled status.
  --
  x | "INSUFFICIENT BALANCE" `T.isPrefixOf` x -> Right Cancelled
  "POSTONLY CANCELED" -> Right PostOnlyCancelled
  "RSN_DUST" -> Right RsnDust
  "RSN_PAUSE" -> Right RsnPause
  x -> Left $ TryFromException x Nothing

newtype AffCode = AffCode
  { unAffCode :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      ToJSON,
      ToJSONKey,
      FromJSON,
      FromJSONKey,
      HasCodec,
      HasItemCodec
    )
  deriving stock
    ( Data,
      Generic
    )

-- $trading
-- Data related to trading and money.

newtype RebateRate = RebateRate
  { unRebateRate :: Rational
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

newCurrencyCode :: (MonadThrow m) => Text -> m CurrencyCode
newCurrencyCode raw =
  case T.strip raw of
    x | length x >= 3 -> pure . CurrencyCode $ T.toUpper x
    _ -> throw $ TryFromException @Text @CurrencyCode raw Nothing

data CurrencyPair = CurrencyPair
  { currencyPairBase :: CurrencyCode,
    currencyPairQuote :: CurrencyCode
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericType CurrencyPair

instance FromJSON CurrencyPair where
  parseJSON =
    withText (inspectType @CurrencyPair)
      $ either (fail . inspect) pure
      . newCurrencyPair

instance FromJSONKey CurrencyPair where
  fromJSONKey =
    A.FromJSONKeyTextParser
      $ parseJSON
      . A.String

currencyPairCon ::
  ( MonadThrow m
  ) =>
  Tagged 'Base CurrencyCode ->
  Tagged 'Quote CurrencyCode ->
  m CurrencyPair
currencyPairCon (Tagged base) (Tagged quote) =
  if unCurrencyCode base /= unCurrencyCode quote
    then pure $ CurrencyPair base quote
    else throw $ TryFromException @CurrencyCode @CurrencyCode base Nothing

instance ToRequestParam CurrencyPair where
  toTextParam x =
    "t"
      <> (unCurrencyCode $ currencyPairBase x :: Text)
      <> (unCurrencyCode $ currencyPairQuote x :: Text)

--
-- TODO : better parsing with advanced regex
--
newCurrencyPair :: (MonadThrow m) => Text -> m CurrencyPair
newCurrencyPair raw =
  case T.splitOn ":" bq0 of
    [bq1]
      | length bq1 == 6 -> do
          let (b1, q1) = T.splitAt 3 bq1
          base <- newCurrencyCode b1
          quote <- newCurrencyCode q1
          currencyPairCon (Tagged @'Base base) (Tagged @'Quote quote)
    [b1, q1]
      | length b1 >= 3 && length q1 >= 3 -> do
          base <- newCurrencyCode b1
          quote <- newCurrencyCode q1
          currencyPairCon (Tagged @'Base base) (Tagged @'Quote quote)
    _ ->
      throw
        $ TryFromException @Text @CurrencyPair raw Nothing
  where
    naked = T.strip raw
    (pre, post) = T.splitAt 1 naked
    bq0 =
      if pre == "t" && length post >= 6
        then post
        else naked

data CurrencyPairConf = CurrencyPairConf
  { currencyPairPrecision :: Natural,
    currencyPairInitMargin :: Rational,
    currencyPairMinMargin :: Rational,
    currencyPairMaxOrderBaseAmt :: MoneyAmount,
    currencyPairMinOrderBaseAmt :: MoneyAmount
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data Candle = Candle
  { candleAt :: UTCTime,
    candleOpen :: QuotePerBase,
    candleClose :: QuotePerBase,
    candleHigh :: QuotePerBase,
    candleLow :: QuotePerBase,
    candleBaseVolume :: MoneyAmount
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data CandleTimeFrame
  = Ctf1m
  | Ctf5m
  | Ctf15m
  | Ctf30m
  | Ctf1h
  | Ctf3h
  | Ctf6h
  | Ctf12h
  | Ctf1D
  | Ctf1W
  | Ctf14D
  | Ctf1M
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic,
      Enum,
      Bounded
    )
  deriving
    ( HasCodec,
      HasItemCodec
    )
    via GenericEnum CandleTimeFrame

instance NFData CandleTimeFrame

instance ToRequestParam CandleTimeFrame where
  toTextParam =
    T.pack
      . drop 3
      . Prelude.show

data Ticker = Ticker
  { tickerSymbol :: CurrencyPair,
    tickerBaseVolume :: MoneyAmount,
    tickerBidBuy :: QuotePerBase,
    tickerAskSell :: QuotePerBase
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

-- $misc
-- General utility data used elsewhere.

newtype RawResponse = RawResponse
  { unRawResponse :: BL.ByteString
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Data,
      Generic
    )

data PltStatus
  = PltOperative
  | PltMaintenance
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

data Error
  = ErrorWebException HttpException
  | ErrorWebPub Web.Request RawResponse (Web.Response BL.ByteString)
  | ErrorWebPrv BL.ByteString Web.Request RawResponse (Web.Response BL.ByteString)
  | ErrorParser Web.Request (Web.Response BL.ByteString) Text
  | ErrorMath Text
  | ErrorTryFrom SomeException
  | ErrorMissingOrder OrderId
  | ErrorUnverifiedOrder (Tagged 'Local Order) (Tagged 'Remote Order)
  | ErrorRemoteOrderState Order
  | ErrorTrading CurrencyCode Text
  deriving stock
    ( Show,
      Generic
    )

instance Exception Error

emptyReq :: Map Int Int
emptyReq = mempty

eradicateNull :: A.Value -> A.Value
eradicateNull = \case
  A.Object xs -> A.Object $ A.mapMaybe devastateNull xs
  A.Array xs -> A.Array $ V.mapMaybe devastateNull xs
  x -> x
  where
    devastateNull =
      \case
        A.Null -> Nothing
        x -> Just $ eradicateNull x
