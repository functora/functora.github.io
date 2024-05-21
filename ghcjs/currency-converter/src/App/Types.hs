{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Unique (..),
    Action (..),
    St (..),
    Money (..),
    Currency (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    Asset (..),
    TopOrBottom (..),
    HeaderOrFooter (..),
    OnlineOrOffline (..),
    StaticOrDynamic (..),
    LeadingOrTrailing (..),
    OpenedOrClosed (..),
    newModel,
    newUnique,
    newUniqueDuplicator,
    pureUpdate,
    newUniqueState,
    newIdentityState,
    newPaymentMethod,
    newAsset,
    FieldType (..),
    htmlFieldType,
    userFieldType,
    DynamicField (..),
    parseDynamicField,
    inspectDynamicField,
    Field (..),
    newField,
    FieldPair (..),
    newFieldPair,
  )
where

import Data.Functor.Barbie
import qualified Data.Generics as Syb
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Cfg
import Functora.Money hiding (Currency, Money)
import qualified Functora.Money as Money
import Functora.Prelude hiding (Field (..))
import Functora.Rates
import qualified Material.Snackbar as Snackbar
import Miso hiding (view)

data Model = Model
  { modelHide :: Bool,
    modelMenu :: OpenedOrClosed,
    modelState :: St Unique,
    modelMarket :: MVar Market,
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelSnackbarQueue :: Snackbar.Queue Action,
    modelProducerQueue :: TChan (ChanItem (Model -> Model)),
    modelConsumerQueue :: TChan (ChanItem (Model -> Model)),
    modelOnlineAt :: UTCTime
  }
  deriving stock (Eq, Generic)

data Unique a = Unique
  { uniqueUid :: Uid,
    uniqueValue :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Action
  = Noop
  | InitUpdate
  | TimeUpdate
  | ChanUpdate Model
  | PushUpdate (JSM (ChanItem (Model -> Model)))

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
    Eq (f Text),
    Ord (f Text),
    Show (f Text),
    Data (f Text)
  )

data St f = St
  { stateScreen :: Screen,
    stateTopMoney :: Money f,
    stateBottomMoney :: Money f,
    stateTopOrBottom :: TopOrBottom,
    stateFieldPairs :: [FieldPair DynamicField f],
    stateAssets :: [Asset f],
    statePaymentMethods :: [PaymentMethod f],
    statePwd :: Field Text f,
    stateEditable :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

deriving via GenericType (St Identity) instance ToJSON (St Identity)

deriving via GenericType (St Identity) instance FromJSON (St Identity)

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

deriving via GenericType (Money Identity) instance ToJSON (Money Identity)

deriving via GenericType (Money Identity) instance FromJSON (Money Identity)

data Currency f = Currency
  { currencyInput :: Field Text f,
    currencyOutput :: CurrencyInfo,
    currencyOpen :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (Currency f)

deriving stock instance (Hkt f) => Ord (Currency f)

deriving stock instance (Hkt f) => Show (Currency f)

deriving stock instance (Hkt f) => Data (Currency f)

instance FunctorB Currency

instance TraversableB Currency

deriving via GenericType (Currency Identity) instance Binary (Currency Identity)

deriving via GenericType (Currency Identity) instance ToJSON (Currency Identity)

deriving via
  GenericType (Currency Identity)
  instance
    FromJSON (Currency Identity)

data PaymentMethod f = PaymentMethod
  { paymentMethodMoney :: Money f,
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

deriving via
  GenericType (PaymentMethod Identity)
  instance
    ToJSON (PaymentMethod Identity)

deriving via
  GenericType (PaymentMethod Identity)
  instance
    FromJSON (PaymentMethod Identity)

data ChanItem a = ChanItem
  { chanItemDelay :: Natural,
    chanItemValue :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Screen
  = Converter
  | Editor
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType Screen

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType TopOrBottom

data HeaderOrFooter
  = Header
  | Footer
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType HeaderOrFooter

data OnlineOrOffline
  = Online
  | Offline
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType OnlineOrOffline

data StaticOrDynamic
  = Static
  | Dynamic
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType StaticOrDynamic

data LeadingOrTrailing
  = Leading
  | Trailing
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType LeadingOrTrailing

data OpenedOrClosed
  = Opened
  | Closed
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType OpenedOrClosed

data Asset f = Asset
  { assetPrice :: Money f,
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

deriving via GenericType (Asset Identity) instance ToJSON (Asset Identity)

deriving via GenericType (Asset Identity) instance FromJSON (Asset Identity)

--
-- TODO : simplify this
--
newModel :: (MonadThrow m, MonadUnliftIO m) => m Model
newModel = do
  let defaultScreen = Editor
  ct <- getCurrentTime
  prod <- liftIO newBroadcastTChanIO
  cons <- liftIO . atomically $ dupTChan prod
  market <- newMarket
  let btc =
        CurrencyInfo
          { currencyInfoCode = CurrencyCode "btc",
            currencyInfoText = mempty
          }
  let usd =
        CurrencyInfo
          { currencyInfoCode = CurrencyCode "usd",
            currencyInfoText = mempty
          }
  topMoney <- newMoney 0 btc
  bottomMoney <- newMoney 0 usd
  issuer <- newFieldPair "Issuer" $ DynamicFieldText "Alice LLC"
  client <- newFieldPair "Client" $ DynamicFieldText "Bob"
  asset <- newAsset "Description" "Jeans" 100 usd
  paymentMethod <- newPaymentMethod 0 btc
  pwd <- newField FieldTypePwd mempty id
  let st =
        Model
          { modelHide = True,
            modelMenu = Closed,
            modelState =
              St
                { stateScreen = defaultScreen,
                  stateTopMoney = topMoney,
                  stateBottomMoney = bottomMoney,
                  stateTopOrBottom = Top,
                  stateFieldPairs = [issuer, client],
                  stateAssets = [asset],
                  statePaymentMethods = [paymentMethod],
                  statePwd = pwd,
                  stateEditable = True
                },
            modelMarket = market,
            modelCurrencies = [btc, usd],
            modelSnackbarQueue = Snackbar.initialQueue,
            modelProducerQueue = prod,
            modelConsumerQueue = cons,
            modelOnlineAt = ct
          }
  fmap (fromRight st) . tryMarket . withMarket market $ do
    currenciesInfo <- currenciesList <$> getCurrencies
    baseCur <-
      fmap (fromRight $ NonEmpty.head currenciesInfo)
        . tryMarket
        . getCurrencyInfo
        $ currencyInfoCode btc
    quoteCur <-
      fmap (fromRight $ NonEmpty.last currenciesInfo)
        . tryMarket
        . getCurrencyInfo
        $ currencyInfoCode usd
    let baseAmt = Tagged 1 :: Money.Money (Tags 'Signed |+| 'Base |+| 'MoneyAmount)
    quote <-
      getQuote
        (Funds baseAmt $ currencyInfoCode baseCur)
        $ currencyInfoCode quoteCur
    let quoteAmt = quoteMoneyAmount quote
    pure
      $ st
      --
      -- Converter
      --
      & #modelState
      . #stateTopMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #stateTopMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged baseAmt
      & #modelState
      . #stateTopMoney
      . #moneyCurrency
      . #currencyOutput
      .~ baseCur
      & #modelState
      . #stateBottomMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelState
      . #stateBottomMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged quoteAmt
      & #modelState
      . #stateBottomMoney
      . #moneyCurrency
      . #currencyOutput
      .~ quoteCur
      --
      -- Editor
      --
      & #modelState
      . #statePaymentMethods
      . each
      . #paymentMethodMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #statePaymentMethods
      . each
      . #paymentMethodMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged baseAmt
      & #modelState
      . #statePaymentMethods
      . each
      . #paymentMethodMoney
      . #moneyCurrency
      . #currencyOutput
      .~ baseCur
      --
      -- Asset
      --
      & #modelState
      . #stateAssets
      . each
      . #assetPrice
      . #moneyCurrency
      . #currencyOutput
      .~ quoteCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newRatioField :: (MonadIO m) => Rational -> m (Field Rational Unique)
newRatioField output =
  newField FieldTypeNumber output inspectRatioDef

newTextField :: (MonadIO m) => Text -> m (Field Text Unique)
newTextField output =
  newField FieldTypeText output id

newDynamicField :: (MonadIO m) => DynamicField -> m (Field DynamicField Unique)
newDynamicField output =
  newField
    ( case output of
        DynamicFieldNumber {} -> FieldTypeNumber
        DynamicFieldText {} -> FieldTypeText
    )
    output
    inspectDynamicField

newCurrency :: (MonadIO m) => CurrencyInfo -> m (Currency Unique)
newCurrency cur =
  Currency
    <$> newTextField mempty
    <*> pure cur
    <*> pure False

newMoney :: (MonadIO m) => Rational -> CurrencyInfo -> m (Money Unique)
newMoney amt cur =
  Money
    <$> newRatioField amt
    <*> newCurrency cur

--
-- NOTE : In most cases we don't need JSM.
--
pureUpdate :: Natural -> (Model -> Model) -> Action
pureUpdate delay =
  PushUpdate
    . pure
    . ChanItem delay

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

newPaymentMethod ::
  ( MonadIO m
  ) =>
  Rational ->
  CurrencyInfo ->
  m (PaymentMethod Unique)
newPaymentMethod amt cur = do
  address <- newFieldPair "Address" $ DynamicFieldText mempty
  notes <- newFieldPair "Details" $ DynamicFieldText mempty
  PaymentMethod
    <$> newMoney amt cur
    <*> pure [address, notes]
    <*> pure Closed

newAsset ::
  (MonadIO m) => Text -> Text -> Rational -> CurrencyInfo -> m (Asset Unique)
newAsset label value amt cur = do
  qty <- newFieldPair "Quantity" $ DynamicFieldNumber 1
  desc <- newFieldPair label $ DynamicFieldText value
  Asset
    <$> newMoney amt cur
    <*> pure [qty, desc]
    <*> pure Closed

newIdentityState :: St Unique -> St Identity
newIdentityState =
  bmap (Identity . uniqueValue)

newUniqueState :: (MonadIO m) => St Identity -> m (St Unique)
newUniqueState =
  btraverse (newUnique . runIdentity)

data FieldType
  = -- Rational
    FieldTypeNumber
  | FieldTypePercent
  | -- Text
    FieldTypeText
  | FieldTypeQrCode
  | FieldTypeLink
  | FieldTypeHtml
  | FieldTypePwd
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType FieldType

htmlFieldType :: FieldType -> Text
htmlFieldType = \case
  FieldTypeNumber -> "number"
  FieldTypePercent -> "number"
  FieldTypeText -> "text"
  FieldTypeQrCode -> "text"
  FieldTypeLink -> "text"
  FieldTypeHtml -> "text"
  FieldTypePwd -> "password"

userFieldType :: FieldType -> Text
userFieldType = \case
  FieldTypeNumber -> "Number"
  FieldTypePercent -> "Percent"
  FieldTypeText -> "Text"
  FieldTypeQrCode -> "QR code"
  FieldTypeLink -> "Link"
  FieldTypeHtml -> "HTML"
  FieldTypePwd -> "Password"

data DynamicField
  = DynamicFieldText Text
  | DynamicFieldNumber Rational
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (Binary, ToJSON, FromJSON) via GenericType DynamicField

parseDynamicField :: Field DynamicField Unique -> Maybe DynamicField
parseDynamicField value =
  case value ^. #fieldType of
    FieldTypeNumber -> DynamicFieldNumber <$> parseRatio input
    FieldTypePercent -> DynamicFieldNumber <$> parseRatio input
    _ -> Just $ DynamicFieldText input
  where
    input = value ^. #fieldInput . #uniqueValue

inspectDynamicField :: DynamicField -> Text
inspectDynamicField = \case
  DynamicFieldText x -> x
  DynamicFieldNumber x -> inspectRatioDef x

data Field a f = Field
  { fieldType :: FieldType,
    fieldInput :: f Text,
    fieldOutput :: a,
    fieldAddCopy :: Bool,
    fieldModalState :: OpenedOrClosed
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

deriving via
  GenericType (Field a Identity)
  instance
    (Typ a, ToJSON a) => ToJSON (Field a Identity)

deriving via
  GenericType (Field a Identity)
  instance
    (Typ a, FromJSON a) => FromJSON (Field a Identity)

newField :: (MonadIO m) => FieldType -> a -> (a -> Text) -> m (Field a Unique)
newField typ output newInput = do
  input <- newUnique $ newInput output
  pure
    Field
      { fieldType = typ,
        fieldInput = input,
        fieldOutput = output,
        fieldAddCopy = True,
        fieldModalState = Closed
      }

data FieldPair a f = FieldPair
  { fieldPairKey :: Field Text f,
    fieldPairValue :: Field a f,
    fieldPairValuePlainText :: Bool,
    fieldPairValueQrCode :: Bool,
    fieldPairValueLink :: Bool,
    fieldPairValueHtml :: Bool
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

deriving via
  GenericType (FieldPair a Identity)
  instance
    (Typ a, ToJSON a) => ToJSON (FieldPair a Identity)

deriving via
  GenericType (FieldPair a Identity)
  instance
    (Typ a, FromJSON a) => FromJSON (FieldPair a Identity)

newFieldPair ::
  (MonadIO m) => Text -> DynamicField -> m (FieldPair DynamicField Unique)
newFieldPair key val =
  FieldPair
    <$> newTextField key
    <*> newDynamicField val
    <*> pure True
    <*> pure False
    <*> pure False
    <*> pure False
