{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Bfx.Data.Type
  ( -- * Orders
    -- $orders
    OrderId (..),
    OrderClientId (..),
    OrderGroupId (..),
    Order (..),
    SomeOrder (..),
    OrderFlag (..),
    OrderFlagAcc (..),
    unOrderFlag,
    unOrderFlagSet,
    OrderStatus (..),
    newOrderStatus,

    -- * Trading
    -- $trading
    FeeRate (..),
    RebateRate (..),
    ProfitRate (..),
    ProfitRateB (..),
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
    PltStatus (..),
    Error (..),
    tryErrorE,
    tryErrorT,
    tryFromE,
    tryFromT,
  )
where

import Bfx.Class.ToRequestParam
import Bfx.Data.Kind
import Bfx.Data.Metro
import Bfx.Import.External
import Bfx.Orphan ()
import Bfx.Util
import Data.Aeson (withText)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import Language.Haskell.TH.Syntax as TH (Lift)
import qualified Network.HTTP.Client as Web
import qualified Text.PrettyPrint as PrettyPrint
import qualified Witch

-- $orders
-- Order data received from Bitfinex
-- and types related to orders.

newtype OrderId
  = OrderId Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance From Natural OrderId

instance From OrderId Natural

instance TryFrom Int64 OrderId where
  tryFrom =
    from @Natural `composeTryRhs` tryFrom

instance TryFrom OrderId Int64 where
  tryFrom =
    tryFrom @Natural `composeTryLhs` from

newtype OrderClientId
  = OrderClientId Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance From Natural OrderClientId

instance From OrderClientId Natural

newtype OrderGroupId
  = OrderGroupId Natural
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

instance From Natural OrderGroupId

instance From OrderGroupId Natural

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom (P.Key a) OrderClientId
  where
  tryFrom =
    from @Natural
      `composeTryRhs` tryFrom

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom OrderClientId (P.Key a)
  where
  tryFrom =
    tryFrom @Natural
      `composeTryLhs` from

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom (P.Key a) OrderGroupId
  where
  tryFrom =
    from @Natural
      `composeTryRhs` tryFrom

instance
  (P.ToBackendKey P.SqlBackend a) =>
  TryFrom OrderGroupId (P.Key a)
  where
  tryFrom =
    tryFrom @Natural
      `composeTryLhs` from

data Order (act :: ExchangeAction) (loc :: Location) = Order
  { orderId :: OrderId,
    orderGroupId :: Maybe OrderGroupId,
    -- | Field might be auto-generated by Bitfinex in case where
    -- it was not provided through 'Bfx.Data.SubmitOrder.Options'.
    orderClientId :: Maybe OrderClientId,
    orderAmount :: Money 'Base act,
    orderSymbol :: CurrencyPair,
    orderRate :: QuotePerBase act,
    orderStatus :: OrderStatus
  }
  deriving stock
    ( Eq,
      Show,
      Generic
    )

data SomeOrder (loc :: Location)
  = forall (act :: ExchangeAction).
    ( Show (Order act loc),
      SingI act
    ) =>
    SomeOrder
      (Sing act)
      (Order act loc)

deriving stock instance Show (SomeOrder loc)

instance Eq (SomeOrder loc) where
  (SomeOrder sx x) == (SomeOrder sy y) =
    case testEquality sx sy of
      Just Refl -> x == y
      Nothing -> False

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
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
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
      Generic,
      Enum,
      Bounded
    )

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

-- $trading
-- Data related to trading and money.

newtype
  FeeRate
    (mrel :: MarketRelation)
    (crel :: CurrencyRelation) = FeeRate
  { unFeeRate :: Rational
  }
  deriving newtype
    ( Eq,
      Ord,
      Show
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

instance From (FeeRate mrel crel) Rational

instance TryFrom Rational (FeeRate mrel crel) where
  tryFrom x
    | x >= 0 && x < 1 = Right $ FeeRate x
    | otherwise = Left $ TryFromException x Nothing

deriving via
  Rational
  instance
    ( Typeable mrel,
      Typeable crel
    ) =>
    PersistFieldSql (FeeRate mrel crel)

instance
  ( Typeable mrel,
    Typeable crel
  ) =>
  PersistField (FeeRate mrel crel)
  where
  toPersistValue =
    PersistRational . from
  fromPersistValue raw =
    case raw of
      PersistRational x ->
        first (const failure) $ tryFrom x
      _ ->
        Left failure
    where
      failure =
        showType @(FeeRate mrel crel)
          <> " PersistValue is invalid "
          <> show raw

newtype RebateRate (mrel :: MarketRelation)
  = RebateRate Rational
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Num
    )
  deriving stock
    ( Generic
    )

instance From (RebateRate mrel) Rational

instance From Rational (RebateRate mrel)

newtype ProfitRate = ProfitRate
  { unProfitRate :: Rational
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      NFData
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

instance TryFrom Rational ProfitRate where
  tryFrom x
    | x > 0 = Right $ ProfitRate x
    | otherwise = Left $ TryFromException x Nothing

instance From ProfitRate Rational

instance FromJSON ProfitRate where
  parseJSON = withText
    (showType @ProfitRate)
    $ \raw -> do
      case tryReadViaRatio @Rational raw of
        Left x -> fail $ show x
        Right x -> pure x

newtype ProfitRateB (b :: Boundary) = ProfitRateB
  { unProfitRateB :: ProfitRate
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      NFData,
      FromJSON
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

newtype CurrencyCode (crel :: CurrencyRelation) = CurrencyCode
  { unCurrencyCode :: Text
  }
  deriving newtype
    ( Eq,
      Ord,
      Show,
      ToJSON,
      NFData
    )
  deriving stock
    ( Generic,
      TH.Lift
    )

instance From (CurrencyCode crel0) (CurrencyCode crel1)

instance (Typeable crel) => FromJSON (CurrencyCode crel) where
  parseJSON = withText
    (showType @(CurrencyCode crel))
    $ \raw -> do
      case newCurrencyCode raw of
        Left x -> fail $ show x
        Right x -> pure x

deriving via
  Text
  instance
    ( Typeable crel
    ) =>
    PersistFieldSql (CurrencyCode crel)

instance
  ( Typeable crel
  ) =>
  PersistField (CurrencyCode crel)
  where
  toPersistValue =
    PersistText . coerce
  fromPersistValue raw =
    case raw of
      PersistText x ->
        first (const failure) $ newCurrencyCode x
      _ ->
        Left failure
    where
      failure =
        showType @(CurrencyCode crel)
          <> " PersistValue is invalid "
          <> show raw

newCurrencyCode ::
  forall crel.
  Text ->
  Either
    (TryFromException Text (CurrencyCode crel))
    (CurrencyCode crel)
newCurrencyCode raw =
  case T.strip raw of
    x | length x >= 3 -> Right . CurrencyCode $ T.toUpper x
    _ -> Left $ TryFromException raw Nothing

data CurrencyPair = CurrencyPair
  { currencyPairBase :: CurrencyCode 'Base,
    currencyPairQuote :: CurrencyCode 'Quote
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic,
      TH.Lift
    )

instance NFData CurrencyPair

instance Out CurrencyPair where
  docPrec = const currencyPairDoc
  doc = currencyPairDoc

currencyPairDoc :: CurrencyPair -> PrettyPrint.Doc
currencyPairDoc x =
  PrettyPrint.text
    . T.unpack
    $ unCurrencyCode (currencyPairBase x)
    <> unCurrencyCode (currencyPairQuote x)

instance FromJSON CurrencyPair where
  parseJSON = withText
    (showType @CurrencyPair)
    $ \raw -> do
      case newCurrencyPair raw of
        Left x -> fail $ show x
        Right x -> pure x

instance FromJSONKey CurrencyPair where
  fromJSONKey =
    A.FromJSONKeyTextParser
      $ parseJSON
      . A.String

currencyPairCon ::
  CurrencyCode 'Base ->
  CurrencyCode 'Quote ->
  Either
    ( TryFromException
        ( CurrencyCode 'Base,
          CurrencyCode 'Quote
        )
        CurrencyPair
    )
    CurrencyPair
currencyPairCon base quote =
  if unCurrencyCode base == unCurrencyCode quote
    then
      Left
        $ TryFromException (base, quote) Nothing
    else
      Right
        $ CurrencyPair base quote

instance ToRequestParam CurrencyPair where
  toTextParam x =
    "t"
      <> (coerce $ currencyPairBase x :: Text)
      <> (coerce $ currencyPairQuote x :: Text)

--
-- TODO : better parsing with advanced regex
--
newCurrencyPair ::
  Text ->
  Either (TryFromException Text CurrencyPair) CurrencyPair
newCurrencyPair raw =
  case T.splitOn ":" bq0 of
    [bq1]
      | length bq1 == 6 -> do
          let (b1, q1) = T.splitAt 3 bq1
          base <- withFirst $ newCurrencyCode b1
          quote <- withFirst $ newCurrencyCode q1
          withFirst $ currencyPairCon base quote
    [b1, q1]
      | length b1 >= 3 && length q1 >= 3 -> do
          base <- withFirst $ newCurrencyCode b1
          quote <- withFirst $ newCurrencyCode q1
          withFirst $ currencyPairCon base quote
    _ ->
      Left
        $ TryFromException raw Nothing
  where
    naked = T.strip raw
    (pre, post) = T.splitAt 1 naked
    bq0 =
      if pre == "t" && length post >= 6
        then post
        else naked
    withFirst ::
      Either (TryFromException source target) a ->
      Either (TryFromException Text CurrencyPair) a
    withFirst =
      first
        $ withTarget @CurrencyPair
        . withSource raw

data CurrencyPairConf = CurrencyPairConf
  { currencyPairPrecision :: Natural,
    currencyPairInitMargin :: Rational,
    currencyPairMinMargin :: Rational,
    currencyPairMaxOrderAmt :: Money 'Base 'Buy,
    currencyPairMinOrderAmt :: Money 'Base 'Buy
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

data Candle = Candle
  { candleAt :: UTCTime,
    candleOpen :: QuotePerBase 'Buy,
    candleClose :: QuotePerBase 'Buy,
    candleHigh :: QuotePerBase 'Buy,
    candleLow :: QuotePerBase 'Buy,
    candleVolume :: Money 'Quote 'Buy
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance NFData Candle

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
      Generic
    )

instance NFData CandleTimeFrame

instance ToRequestParam CandleTimeFrame where
  toTextParam =
    T.pack
      . drop 3
      . show

data Ticker = Ticker
  { tickerSymbol :: CurrencyPair,
    tickerVolume :: Money 'Base 'Buy,
    tickerBid :: QuotePerBase 'Buy,
    tickerAsk :: QuotePerBase 'Sell
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance NFData Ticker

-- $misc
-- General utility data used elsewhere.

data PltStatus
  = PltOperative
  | PltMaintenance
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

instance Out PltStatus

data Error
  = ErrorWebException HttpException
  | ErrorWebPub Web.Request (Web.Response ByteString)
  | ErrorWebPrv ByteString Web.Request (Web.Response ByteString)
  | ErrorParser Web.Request (Web.Response ByteString) Text
  | ErrorMath Text
  | ErrorTryFrom SomeException
  | ErrorMissingOrder OrderId
  | ErrorUnverifiedOrder (SomeOrder 'Local) (SomeOrder 'Remote)
  | ErrorOrderState (SomeOrder 'Remote)
  | ErrorTrading (CurrencyCode 'Quote) Text
  deriving stock
    ( Show,
      Generic
    )

tryErrorE ::
  forall source target.
  ( Show source,
    Typeable source,
    Typeable target
  ) =>
  Either (TryFromException source target) target ->
  Either Error target
tryErrorE =
  first
    $ ErrorTryFrom
    . SomeException

tryErrorT ::
  forall source target m.
  ( Show source,
    Typeable source,
    Typeable target,
    Monad m
  ) =>
  Either (TryFromException source target) target ->
  ExceptT Error m target
tryErrorT =
  except . tryErrorE

tryFromE ::
  forall source target.
  ( Show source,
    Typeable source,
    Typeable target,
    TryFrom source target,
    'False ~ (source == target)
  ) =>
  source ->
  Either Error target
tryFromE =
  tryErrorE . tryFrom

tryFromT ::
  forall source target m.
  ( Show source,
    Typeable source,
    Typeable target,
    TryFrom source target,
    Monad m,
    'False ~ (source == target)
  ) =>
  source ->
  ExceptT Error m target
tryFromT =
  except . tryFromE