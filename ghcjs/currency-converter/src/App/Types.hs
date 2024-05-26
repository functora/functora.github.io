{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Unique (..),
    Action (..),
    St (..),
    StConv (..),
    StDoc (..),
    StCrypto (..),
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

import qualified Data.ByteString.Base64.URL as B64URL
import Data.Functor.Barbie
import qualified Data.Generics as Syb
import qualified Data.List.NonEmpty as NonEmpty
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Money hiding (Currency, Money)
import qualified Functora.Money as Money
import Functora.Prelude hiding (Field (..))
import Functora.Rates
import qualified Material.Snackbar as Snackbar
import Miso hiding (URI, view)
import qualified Text.URI as URI

data Model = Model
  { modelHide :: Bool,
    modelMenu :: OpenedOrClosed,
    modelState :: St Unique,
    --
    -- TODO : modelStateHistory :: [St Unique]
    --
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
  { stScreen :: Screen,
    stConv :: StConv f,
    stDoc :: StDoc f,
    stIkm :: Field Text f,
    stKm :: Aes.Km,
    stCrypto :: Maybe (StCrypto f)
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

data StCrypto f = StCrypto
  { stCryptoKm :: Aes.Km,
    stCryptoIkm :: Field Text f,
    stCryptoDoc :: Aes.Crypto
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StCrypto f)

deriving stock instance (Hkt f) => Ord (StCrypto f)

deriving stock instance (Hkt f) => Show (StCrypto f)

deriving stock instance (Hkt f) => Data (StCrypto f)

instance FunctorB StCrypto

instance TraversableB StCrypto

deriving via GenericType (StCrypto Identity) instance Binary (StCrypto Identity)

data StConv f = StConv
  { stConvTopMoney :: Money f,
    stConvBottomMoney :: Money f,
    stConvTopOrBottom :: TopOrBottom
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StConv f)

deriving stock instance (Hkt f) => Ord (StConv f)

deriving stock instance (Hkt f) => Show (StConv f)

deriving stock instance (Hkt f) => Data (StConv f)

instance FunctorB StConv

instance TraversableB StConv

deriving via GenericType (StConv Identity) instance Binary (StConv Identity)

data StDoc f = StDoc
  { stDocFieldPairs :: [FieldPair DynamicField f],
    stDocAssets :: [Asset f],
    stDocPaymentMethods :: [PaymentMethod f],
    stDocEditable :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StDoc f)

deriving stock instance (Hkt f) => Ord (StDoc f)

deriving stock instance (Hkt f) => Show (StDoc f)

deriving stock instance (Hkt f) => Data (StDoc f)

instance FunctorB StDoc

instance TraversableB StDoc

deriving via GenericType (StDoc Identity) instance Binary (StDoc Identity)

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

data ChanItem a = ChanItem
  { chanItemDelay :: Natural,
    chanItemValue :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Screen
  = Converter
  | Editor
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType Screen

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

data OpenedOrClosed
  = Opened
  | Closed
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType OpenedOrClosed

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

--
-- TODO : simplify this !!!!
--
newModel :: (MonadThrow m, MonadUnliftIO m) => URI -> m Model
newModel uri = do
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
  ikm <- newField FieldTypePwd mempty id
  km <- Aes.randomKm 32
  mcrypto <- parseCrypto uri
  let st =
        Model
          { modelHide = True,
            modelMenu = Closed,
            modelState =
              St
                { stScreen = defaultScreen,
                  stConv =
                    StConv
                      { stConvTopMoney = topMoney,
                        stConvBottomMoney = bottomMoney,
                        stConvTopOrBottom = Top
                      },
                  stDoc =
                    StDoc
                      { stDocFieldPairs = [issuer, client],
                        stDocAssets = [asset],
                        stDocPaymentMethods = [paymentMethod],
                        stDocEditable = True
                      },
                  stIkm = ikm,
                  stKm = km,
                  stCrypto = mcrypto
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
      . #stConv
      . #stConvTopMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #stConv
      . #stConvTopMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged baseAmt
      & #modelState
      . #stConv
      . #stConvTopMoney
      . #moneyCurrency
      . #currencyOutput
      .~ baseCur
      & #modelState
      . #stConv
      . #stConvBottomMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelState
      . #stConv
      . #stConvBottomMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged quoteAmt
      & #modelState
      . #stConv
      . #stConvBottomMoney
      . #moneyCurrency
      . #currencyOutput
      .~ quoteCur
      --
      -- Editor
      --
      & #modelState
      . #stDoc
      . #stDocPaymentMethods
      . each
      . #paymentMethodMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #stDoc
      . #stDocPaymentMethods
      . each
      . #paymentMethodMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged baseAmt
      & #modelState
      . #stDoc
      . #stDocPaymentMethods
      . each
      . #paymentMethodMoney
      . #moneyCurrency
      . #currencyOutput
      .~ baseCur
      --
      -- Asset
      --
      & #modelState
      . #stDoc
      . #stDocAssets
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
  deriving (Binary) via GenericType FieldType

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
  deriving (Binary) via GenericType DynamicField

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

parseCrypto :: (MonadThrow m, MonadIO m) => URI -> m (Maybe (StCrypto Unique))
parseCrypto uri = do
  kKm <- URI.mkQueryKey "k"
  kDoc <- URI.mkQueryKey "d"
  let qs = URI.uriQuery uri
  case (,) <$> asumMap (qsGet kDoc) qs <*> asumMap (qsGet kKm) qs of
    Nothing -> pure Nothing
    Just (vDoc, vKm) -> do
      bKm <- either throwString pure . B64URL.decode $ encodeUtf8 vKm
      bDoc <- either throwString pure . B64URL.decode $ encodeUtf8 vDoc
      km <- either (throwString . thd3) pure $ decodeBinary bKm
      ikm <- newField FieldTypePwd mempty id
      doc <- either (throwString . thd3) pure $ decodeBinary bDoc
      pure
        $ Just
          StCrypto
            { stCryptoKm = km,
              stCryptoIkm = ikm,
              stCryptoDoc = doc
            }

qsGet ::
  URI.RText 'URI.QueryKey ->
  URI.QueryParam ->
  Maybe Text
qsGet qk = \case
  URI.QueryParam k x | k == qk -> Just $ URI.unRText x
  _ -> Nothing
