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
    PltStatus (..),
    Error (..),
  )
where

import Bfx.Class.ToRequestParam
import Bfx.Data.Kind
import Bfx.Import.External
import Bfx.Orphan ()
import Data.Aeson (withText)
import qualified Data.Aeson as A
import qualified Data.Text as T
import Language.Haskell.TH.Syntax as TH (Lift)
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
      Num,
      ToJSON,
      FromJSON
    )
  deriving stock
    ( Generic
    )

newtype OrderClientId = OrderClientId
  { unOrderClientId :: Natural
  }
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

newtype OrderGroupId = OrderGroupId
  { unOrderGroupId :: Natural
  }
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

data Order (act :: BuyOrSell) (loc :: LocalOrRemote) = Order
  { orderId :: OrderId,
    orderGroupId :: Maybe OrderGroupId,
    -- | Field might be auto-generated by Bitfinex in case where
    -- it was not provided through 'Bfx.Data.SubmitOrder.Options'.
    orderClientId :: Maybe OrderClientId,
    orderAmount :: Money (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount |+| act),
    orderSymbol :: CurrencyPair,
    orderRate :: Money (Tags 'Unsigned |+| 'QuotePerBase |+| act),
    orderStatus :: OrderStatus
  }
  deriving stock
    ( Eq,
      Show,
      Generic
    )

data SomeOrder (loc :: LocalOrRemote)
  = forall (act :: BuyOrSell).
    ( Show (Order act loc),
      SingI act,
      Typeable act
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
      Data,
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

-- newtype
--   FeeRate
--     (mrel :: MakerOrTaker)
--     (crel :: CurrencyRelation) = FeeRate
--   { unFeeRate :: Rational
--   }
--   deriving newtype
--     ( Eq,
--       Ord,
--       Show
--     )
--   deriving stock
--     ( Generic,
--       TH.Lift
--     )
--
-- instance From (FeeRate mrel crel) Rational
--
-- instance TryFrom Rational (FeeRate mrel crel) where
--   tryFrom x
--     | x >= 0 && x < 1 = Right $ FeeRate x
--     | otherwise = Left $ TryFromException x Nothing
--
-- deriving via
--   Rational
--   instance
--     ( Typeable mrel,
--       Typeable crel
--     ) =>
--     PersistFieldSql (FeeRate mrel crel)
--
-- instance
--   ( Typeable mrel,
--     Typeable crel
--   ) =>
--   PersistField (FeeRate mrel crel)
--   where
--   toPersistValue =
--     PersistRational . from
--   fromPersistValue raw =
--     case raw of
--       PersistRational x ->
--         first (const failure) $ tryFrom x
--       _ ->
--         Left failure
--     where
--       failure =
--         showType @(FeeRate mrel crel)
--           <> " PersistValue is invalid "
--           <> show raw

newtype RebateRate (mrel :: MakerOrTaker)
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

-- newtype ProfitRate = ProfitRate
--   { unProfitRate :: Rational
--   }
--   deriving newtype
--     ( Eq,
--       Ord,
--       Show,
--       NFData
--     )
--   deriving stock
--     ( Generic,
--       TH.Lift
--     )
--
-- instance TryFrom Rational ProfitRate where
--   tryFrom x
--     | x > 0 = Right $ ProfitRate x
--     | otherwise = Left $ TryFromException x Nothing
--
-- instance From ProfitRate Rational
--
-- instance FromJSON ProfitRate where
--   parseJSON =
--     withText (inspectType @ProfitRate)
--       $ either (fail . inspect) (pure . ProfitRate)
--       . parseRatio
--
-- newtype ProfitRateB (b :: MinOrMax) = ProfitRateB
--   { unProfitRateB :: ProfitRate
--   }
--   deriving newtype
--     ( Eq,
--       Ord,
--       Show,
--       NFData,
--       FromJSON
--     )
--   deriving stock
--     ( Generic,
--       TH.Lift
--     )

-- newtype CurrencyCode (crel :: CurrencyRelation) = CurrencyCode
--   { unCurrencyCode :: Text
--   }
--   deriving newtype
--     ( Eq,
--       Ord,
--       Show,
--       ToJSON,
--       NFData
--     )
--   deriving stock
--     ( Generic,
--       TH.Lift
--     )
--
-- instance From (CurrencyCode crel0) (CurrencyCode crel1)
--
-- instance (Typeable crel) => FromJSON (CurrencyCode crel) where
--   parseJSON = withText
--     (showType @(CurrencyCode crel))
--     $ \raw -> do
--       case newCurrencyCode raw of
--         Left x -> fail $ show x
--         Right x -> pure x
--
-- deriving via
--   Text
--   instance
--     ( Typeable crel
--     ) =>
--     PersistFieldSql (CurrencyCode crel)
--
-- instance
--   ( Typeable crel
--   ) =>
--   PersistField (CurrencyCode crel)
--   where
--   toPersistValue =
--     PersistText . coerce
--   fromPersistValue raw =
--     case raw of
--       PersistText x ->
--         first (const failure) $ newCurrencyCode x
--       _ ->
--         Left failure
--     where
--       failure =
--         showType @(CurrencyCode crel)
--           <> " PersistValue is invalid "
--           <> show raw

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
      Generic,
      TH.Lift
    )

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
      <> (coerce $ currencyPairBase x :: Text)
      <> (coerce $ currencyPairQuote x :: Text)

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
    currencyPairMaxOrderAmt ::
      Money (Tags 'Unsigned |+| 'Base |+| 'Max |+| 'MoneyAmount),
    currencyPairMinOrderAmt ::
      Money (Tags 'Unsigned |+| 'Base |+| 'Min |+| 'MoneyAmount)
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

data Candle = Candle
  { candleAt :: UTCTime,
    candleOpen :: Money (Tags 'Unsigned |+| 'QuotePerBase),
    candleClose :: Money (Tags 'Unsigned |+| 'QuotePerBase),
    candleHigh :: Money (Tags 'Unsigned |+| 'QuotePerBase),
    candleLow :: Money (Tags 'Unsigned |+| 'QuotePerBase),
    candleVolume :: Money (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount)
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
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
      Generic
    )

instance NFData CandleTimeFrame

instance ToRequestParam CandleTimeFrame where
  toTextParam =
    T.pack
      . drop 3
      . Prelude.show

data Ticker = Ticker
  { tickerSymbol :: CurrencyPair,
    tickerVolume :: Money (Tags 'Unsigned |+| 'Base |+| 'MoneyAmount),
    tickerBid :: Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Buy),
    tickerAsk :: Money (Tags 'Unsigned |+| 'QuotePerBase |+| 'Sell)
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )

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
  | ErrorTrading CurrencyCode Text
  deriving stock
    ( Show,
      Generic
    )

instance Exception Error
