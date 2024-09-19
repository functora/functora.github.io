{-# LANGUAGE UndecidableInstances #-}

module Functora.Miso.Types
  ( Typ,
    Hkt,
    Unique (..),
    newUnique,
    newUniqueDuplicator,
    Field (..),
    FieldOpts (..),
    defFieldOpts,
    defTruncateLimit,
    newField,
    newFieldId,
    newRatioField,
    newTextField,
    newPasswordField,
    newDynamicField,
    newDynamicFieldId,
    newDynamicTitleField,
    DynamicField (..),
    parseDynamicField,
    inspectDynamicField,
    FieldType (..),
    htmlFieldType,
    userFieldType,
    FieldPair (..),
    newFieldPair,
    newFieldPairId,
    mergeFieldPairs,
    Currency (..),
    newCurrency,
    Money (..),
    newMoney,
    Asset (..),
    newAsset,
    PaymentMethod (..),
    newPaymentMethod,
    Fav (..),
    InstantOrDelayed (..),
    instantOrDelayedTime,
    instantOrDelayedValue,
    drainTChan,
    qsGet,
    uniqueToIdentity,
    identityToUnique,
    keyed,
    TopOrBottom (..),
    HeaderOrFooter (..),
    OnlineOrOffline (..),
    StaticOrDynamic (..),
    LeadingOrTrailing (..),
    FilledOrOutlined (..),
    OpenedOrClosed (..),
    AssetsAndPaymentsLayout (..),
  )
where

import Data.Foldable (foldMap)
import Data.Functor.Barbie
import qualified Data.Generics as Syb
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Money hiding (Currency, Money)
import qualified Miso.Html.Types as Miso
import qualified Text.URI as URI

type Typ a =
  ( Typeable a,
    Eq a,
    Ord a,
    Show a,
    Data a,
    Binary a
  )

type Hkt f =
  ( Typeable f,
    Eq (f Unicode),
    Ord (f Unicode),
    Show (f Unicode),
    Data (f Unicode)
  )

data Unique a = Unique
  { uniqueUid :: Uid,
    uniqueValue :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

instance Foldable Unique where
  foldMap f x = f $ uniqueValue x

instance Foldable1 Unique where
  foldMap1 f x = f $ uniqueValue x

newUnique :: (MonadIO m) => a -> m (Unique a)
newUnique x =
  Unique
    <$> newUid
    <*> pure x

newUniqueDuplicator ::
  forall (b :: Type) a m.
  ( Data a,
    Typeable b,
    MonadIO m
  ) =>
  m (a -> a)
newUniqueDuplicator = do
  uid <- newUid
  pure
    $ Syb.everywhere
    $ Syb.mkT
      ( (& #uniqueUid %~ addUid uid) :: Unique b -> Unique b
      )

data Field a f = Field
  { fieldType :: FieldType,
    fieldInput :: f Unicode,
    fieldOutput :: a,
    fieldModalState :: OpenedOrClosed,
    fieldOpts :: FieldOpts
  }
  deriving stock (Generic)

deriving stock instance (Typ a, Hkt f) => Eq (Field a f)

deriving stock instance (Typ a, Hkt f) => Ord (Field a f)

deriving stock instance (Typ a, Hkt f) => Show (Field a f)

deriving stock instance (Typ a, Hkt f) => Data (Field a f)

instance FunctorB (Field a)

instance TraversableB (Field a)

deriving via
  GenericType (Field a Identity)
  instance
    (Typ a) => Binary (Field a Identity)

data FieldOpts = FieldOpts
  { fieldOptsAllowCopy :: Bool,
    fieldOptsTruncateLimit :: Maybe Int,
    fieldOptsTruncateState :: Maybe OpenedOrClosed,
    fieldOptsQrState :: Maybe OpenedOrClosed
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

deriving via GenericType FieldOpts instance Binary FieldOpts

defFieldOpts :: FieldOpts
defFieldOpts =
  FieldOpts
    { fieldOptsAllowCopy = True,
      fieldOptsTruncateLimit = Just defTruncateLimit,
      fieldOptsTruncateState = Just Closed,
      fieldOptsQrState = Just Closed
    }

defTruncateLimit :: Int
defTruncateLimit = 67

newField ::
  (MonadIO m) => FieldType -> a -> (a -> Unicode) -> m (Field a Unique)
newField typ output newInput = do
  input <- newUnique $ newInput output
  pure
    Field
      { fieldType = typ,
        fieldInput = input,
        fieldOutput = output,
        fieldModalState = Closed,
        fieldOpts = defFieldOpts
      }

newFieldId :: FieldType -> (a -> Unicode) -> a -> Field a Identity
newFieldId typ viewer output =
  Field
    { fieldType = typ,
      fieldInput = Identity $ viewer output,
      fieldOutput = output,
      fieldModalState = Closed,
      fieldOpts = defFieldOpts
    }

newRatioField :: (MonadIO m) => Rational -> m (Field Rational Unique)
newRatioField output =
  newField
    FieldTypeNumber
    output
    inspectRatioDef

newTextField :: (MonadIO m) => Unicode -> m (Field Unicode Unique)
newTextField output =
  newField FieldTypeText output id

newPasswordField :: (MonadIO m) => Unicode -> m (Field Unicode Unique)
newPasswordField output =
  newField FieldTypePassword output id

newDynamicField :: (MonadIO m) => DynamicField -> m (Field DynamicField Unique)
newDynamicField output =
  newField
    ( case output of
        DynamicFieldNumber {} -> FieldTypeNumber
        DynamicFieldText {} -> FieldTypeText
    )
    output
    inspectDynamicField

newDynamicFieldId :: DynamicField -> Field DynamicField Identity
newDynamicFieldId output =
  newFieldId
    ( case output of
        DynamicFieldNumber {} -> FieldTypeNumber
        DynamicFieldText {} -> FieldTypeText
    )
    inspectDynamicField
    output

newDynamicTitleField ::
  (MonadIO m) => Unicode -> m (Field DynamicField Unique)
newDynamicTitleField =
  fmap (& #fieldType .~ FieldTypeTitle)
    . newDynamicField
    . DynamicFieldText

data DynamicField
  = DynamicFieldText Unicode
  | DynamicFieldNumber Rational
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (Binary) via GenericType DynamicField

parseDynamicField :: Field DynamicField Unique -> Maybe DynamicField
parseDynamicField value =
  case value ^. #fieldType of
    FieldTypeNumber -> DynamicFieldNumber <$> parseRatio str
    FieldTypePercent -> DynamicFieldNumber <$> parseRatio str
    _ -> Just $ DynamicFieldText str
  where
    str = value ^. #fieldInput . #uniqueValue

inspectDynamicField :: DynamicField -> Unicode
inspectDynamicField = \case
  DynamicFieldText x -> x
  DynamicFieldNumber x -> inspectRatioDef x

data FieldType
  = -- Rational
    FieldTypeNumber
  | FieldTypePercent
  | -- Textual
    FieldTypeText
  | FieldTypeTitle
  | FieldTypeQrCode
  | FieldTypeHtml
  | FieldTypePassword
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType FieldType

htmlFieldType :: FieldType -> Unicode
htmlFieldType = \case
  FieldTypeNumber -> "number"
  FieldTypePercent -> "number"
  FieldTypeText -> "text"
  FieldTypeTitle -> "text"
  FieldTypeQrCode -> "text"
  FieldTypeHtml -> "text"
  FieldTypePassword -> "password"

userFieldType :: FieldType -> Unicode
userFieldType = \case
  FieldTypeNumber -> "Number"
  FieldTypePercent -> "Percent"
  FieldTypeText -> "Text"
  FieldTypeTitle -> "Title"
  FieldTypeQrCode -> "QR code"
  FieldTypeHtml -> "HTML"
  FieldTypePassword -> "Password"

data FieldPair a f = FieldPair
  { fieldPairKey :: Field Unicode f,
    fieldPairValue :: Field a f
  }
  deriving stock (Generic)

deriving stock instance (Typ a, Hkt f) => Eq (FieldPair a f)

deriving stock instance (Typ a, Hkt f) => Ord (FieldPair a f)

deriving stock instance (Typ a, Hkt f) => Show (FieldPair a f)

deriving stock instance (Typ a, Hkt f) => Data (FieldPair a f)

instance FunctorB (FieldPair a)

instance TraversableB (FieldPair a)

deriving via
  GenericType (FieldPair a Identity)
  instance
    (Typ a) => Binary (FieldPair a Identity)

newFieldPair ::
  (MonadIO m) => Unicode -> DynamicField -> m (FieldPair DynamicField Unique)
newFieldPair key val =
  FieldPair
    <$> newTextField key
    <*> newDynamicField val

newFieldPairId :: Unicode -> DynamicField -> FieldPair DynamicField Identity
newFieldPairId key val =
  FieldPair
    (newFieldId FieldTypeText id key)
    (newDynamicFieldId val)

mergeFieldPairs ::
  ( Foldable1 f
  ) =>
  [FieldPair t f] ->
  [FieldPair t f] ->
  [FieldPair t f]
mergeFieldPairs next prev =
  if ( (length next == length prev)
        && (all (== (mempty, mempty)) prevInputs)
     )
    || (nextInputs == prevInputs)
    then fmap (uncurry merge) $ zip next prev
    else next
  where
    nextInputs :: [(Unicode, Unicode)]
    nextInputs = fmap inputs next
    prevInputs :: [(Unicode, Unicode)]
    prevInputs = fmap inputs prev
    merge :: FieldPair t f -> FieldPair t f -> FieldPair t f
    merge new old =
      old
        & #fieldPairKey
        . #fieldInput
        .~ (new ^. #fieldPairKey . #fieldInput)
        & #fieldPairKey
        . #fieldOutput
        .~ (new ^. #fieldPairKey . #fieldOutput)
        & #fieldPairValue
        . #fieldInput
        .~ (new ^. #fieldPairValue . #fieldInput)
        & #fieldPairValue
        . #fieldOutput
        .~ (new ^. #fieldPairValue . #fieldOutput)
    inputs :: (Foldable1 f) => FieldPair t f -> (Unicode, Unicode)
    inputs x =
      ( fold1 $ x ^. #fieldPairKey . #fieldInput,
        fold1 $ x ^. #fieldPairValue . #fieldInput
      )

data Currency f = Currency
  { currencyInput :: Field Unicode f,
    currencyOutput :: CurrencyInfo,
    currencyModalState :: OpenedOrClosed
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Currency f)

deriving stock instance (Hkt f) => Ord (Currency f)

deriving stock instance (Hkt f) => Show (Currency f)

deriving stock instance (Hkt f) => Data (Currency f)

instance FunctorB Currency

instance TraversableB Currency

deriving via
  GenericType (Currency Identity)
  instance
    Binary (Currency Identity)

newCurrency :: (MonadIO m) => CurrencyInfo -> m (Currency Unique)
newCurrency cur =
  Currency
    <$> newTextField mempty
    <*> pure cur
    <*> pure Closed

data Money f = Money
  { moneyAmount :: Field Rational f,
    moneyCurrency :: Currency f
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Money f)

deriving stock instance (Hkt f) => Ord (Money f)

deriving stock instance (Hkt f) => Show (Money f)

deriving stock instance (Hkt f) => Data (Money f)

instance FunctorB Money

instance TraversableB Money

deriving via GenericType (Money Identity) instance Binary (Money Identity)

newMoney :: (MonadIO m) => Rational -> CurrencyInfo -> m (Money Unique)
newMoney amt cur =
  Money
    <$> newRatioField amt
    <*> newCurrency cur

data Asset f = Asset
  { assetPrice :: Money f,
    assetPriceLabel :: Field Unicode f,
    assetFieldPairs :: [FieldPair DynamicField f],
    assetModalState :: OpenedOrClosed
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Asset f)

deriving stock instance (Hkt f) => Ord (Asset f)

deriving stock instance (Hkt f) => Show (Asset f)

deriving stock instance (Hkt f) => Data (Asset f)

instance FunctorB Asset

instance TraversableB Asset

deriving via GenericType (Asset Identity) instance Binary (Asset Identity)

newAsset ::
  ( MonadIO m
  ) =>
  Unicode ->
  Rational ->
  CurrencyInfo ->
  m (Asset Unique)
newAsset label amt cur = do
  lbl <- newTextField label
  Asset
    <$> newMoney amt cur
    <*> pure lbl
    <*> pure mempty
    <*> pure Closed

data PaymentMethod f = PaymentMethod
  { paymentMethodMoney :: Money f,
    paymentMethodMoneyLabel :: Field Unicode f,
    paymentMethodFieldPairs :: [FieldPair DynamicField f],
    paymentMethodModalState :: OpenedOrClosed
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (PaymentMethod f)

deriving stock instance (Hkt f) => Ord (PaymentMethod f)

deriving stock instance (Hkt f) => Show (PaymentMethod f)

deriving stock instance (Hkt f) => Data (PaymentMethod f)

instance FunctorB PaymentMethod

instance TraversableB PaymentMethod

deriving via
  GenericType (PaymentMethod Identity)
  instance
    Binary (PaymentMethod Identity)

newPaymentMethod ::
  ( MonadIO m
  ) =>
  CurrencyInfo ->
  Maybe Unicode ->
  m (PaymentMethod Unique)
newPaymentMethod cur addr0 = do
  lbl <-
    newTextField $ inspectCurrencyInfo cur <> " total"
  addr1 <-
    maybe
      ( pure Nothing
      )
      ( fmap (Just . (& #fieldPairValue . #fieldType .~ FieldTypeQrCode))
          . newFieldPair (inspectCurrencyInfo cur <> " address")
          . DynamicFieldText
      )
      addr0
  PaymentMethod
    <$> newMoney 0 cur
    <*> pure lbl
    <*> pure (maybeToList addr1)
    <*> pure Closed

data Fav = Fav
  { favUri :: URI,
    favCreatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType Fav

data InstantOrDelayed a
  = Instant a
  | Delayed Natural a
  deriving stock (Eq, Ord, Show, Data, Generic)

instantOrDelayedTime :: InstantOrDelayed a -> Natural
instantOrDelayedTime = \case
  Instant {} -> 0
  Delayed delay _ -> delay

instantOrDelayedValue :: InstantOrDelayed a -> a
instantOrDelayedValue = \case
  Instant x -> x
  Delayed _ x -> x

drainTChan :: (MonadIO m) => TChan (InstantOrDelayed a) -> m [a]
drainTChan chan = do
  item <- liftIO . atomically $ readTChan chan
  liftIO
    . fmap ((instantOrDelayedValue item :) . reverse)
    . drainInto []
    $ instantOrDelayedTime item
  where
    drainInto acc delay = do
      item <- atomically $ tryReadTChan chan
      case item of
        Nothing | delay == 0 -> pure acc
        Nothing -> do
          sleepMilliSeconds $ from @Natural @Integer delay
          drainInto acc 0
        Just next ->
          drainInto (instantOrDelayedValue next : acc)
            . max delay
            $ instantOrDelayedTime next

qsGet :: URI.RText 'URI.QueryKey -> [URI.QueryParam] -> Maybe Text
qsGet key =
  asumMap $ \case
    URI.QueryParam k x | k == key -> Just $ URI.unRText x
    _ -> Nothing

uniqueToIdentity :: (FunctorB f) => f Unique -> f Identity
uniqueToIdentity =
  bmap $ Identity . uniqueValue

identityToUnique :: (TraversableB f, MonadIO m) => f Identity -> m (f Unique)
identityToUnique =
  btraverse $ newUnique . runIdentity

keyed :: Uid -> View action -> View action
keyed uid = \case
  Node x0 x1 Nothing x2 x3
    | not (nullUid uid) ->
        Node
          x0
          x1
          ( Just
              . Miso.Key
              . either impureThrow id
              . decodeUtf8Strict
              . unTagged
              $ htmlUid uid
          )
          x2
          x3
  x ->
    x

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType TopOrBottom

data HeaderOrFooter
  = Header
  | Footer
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType HeaderOrFooter

data OnlineOrOffline
  = Online
  | Offline
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType OnlineOrOffline

data StaticOrDynamic
  = Static
  | Dynamic
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType StaticOrDynamic

data LeadingOrTrailing
  = Leading
  | Trailing
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType LeadingOrTrailing

data FilledOrOutlined
  = Filled
  | Outlined
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType FilledOrOutlined

data OpenedOrClosed
  = Opened
  | Closed
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType OpenedOrClosed

data AssetsAndPaymentsLayout
  = AssetsBeforePayments
  | PaymentsBeforeAssets
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType AssetsAndPaymentsLayout
