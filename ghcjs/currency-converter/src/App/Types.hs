{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Unique (..),
    Action (..),
    St (..),
    StConv (..),
    StDoc (..),
    StExt (..),
    Money (..),
    Currency (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    unQrCode,
    Asset (..),
    TopOrBottom (..),
    HeaderOrFooter (..),
    OnlineOrOffline (..),
    StaticOrDynamic (..),
    LeadingOrTrailing (..),
    FilledOrOutlined (..),
    OpenedOrClosed (..),
    AssetsAndPaymentsLayout (..),
    newUnique,
    newUniqueDuplicator,
    pureUpdate,
    newAsset,
    newPaymentMethod,
    uniqueToIdentity,
    identityToUnique,
    FieldType (..),
    htmlFieldType,
    userFieldType,
    DynamicField (..),
    parseDynamicField,
    inspectDynamicField,
    Field (..),
    newField,
    newRatioField,
    newTextField,
    newDynamicField,
    newDynamicTitleField,
    newPasswordField,
    newMoney,
    FieldPair (..),
    newFieldPair,
    unShareUri,
    stUri,
    stExtUri,
    setScreenPure,
    setScreenAction,
    setExtScreenAction,
    shareLink,
    vsn,
  )
where

import App.Prelude
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Barbie
import qualified Data.Generics as Syb
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Money hiding (Currency, Money)
import qualified Functora.Prelude as Prelude
import Functora.Rates
import qualified Functora.Web as Web
import qualified Material.Snackbar as Snackbar
import Miso hiding (URI, view)
import qualified Paths_app as Paths
import qualified Text.URI as URI

data Model = Model
  { modelHide :: Bool,
    modelMenu :: OpenedOrClosed,
    modelShare :: OpenedOrClosed,
    modelTemplates :: OpenedOrClosed,
    modelExamples :: OpenedOrClosed,
    modelState :: St Unique,
    --
    -- TODO : modelStateHistory :: [St Unique]
    --
    modelMarket :: MVar Market,
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelSnackbarQueue :: Snackbar.Queue Action,
    modelProducerQueue :: TChan (ChanItem (Model -> Model)),
    modelConsumerQueue :: TChan (ChanItem (Model -> Model)),
    modelOnlineAt :: UTCTime,
    modelWebOpts :: Web.Opts
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
  | SyncInputs
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
    stDoc :: StDoc f,
    stIkm :: Field Text f,
    stKm :: Aes.Km,
    stPre :: Field DynamicField f,
    stExt :: Maybe (StExt f)
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (St f)

deriving stock instance (Hkt f) => Ord (St f)

deriving stock instance (Hkt f) => Show (St f)

deriving stock instance (Hkt f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance Binary (St Identity)

data StExt f = StExt
  { stExtKm :: Aes.Km,
    stExtIkm :: Field Text f,
    stExtDoc :: Aes.Crypto,
    stExtPre :: Field DynamicField f,
    stExtScreen :: Screen
  }
  deriving stock (Generic)

deriving stock instance (Hkt f) => Eq (StExt f)

deriving stock instance (Hkt f) => Ord (StExt f)

deriving stock instance (Hkt f) => Show (StExt f)

deriving stock instance (Hkt f) => Data (StExt f)

instance FunctorB StExt

instance TraversableB StExt

deriving via
  GenericType (StExt Identity)
  instance
    Binary (StExt Identity)

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
  { stDocConv :: StConv f,
    stDocFieldPairs :: [FieldPair DynamicField f],
    stDocAssets :: [Asset f],
    stDocPaymentMethods :: [PaymentMethod f],
    stDocFieldPairsHeader :: Field DynamicField f,
    stDocAssetsHeader :: Field DynamicField f,
    stDocPaymentMethodsHeader :: Field DynamicField f,
    stDocAssetsAndPaymentsLayout :: AssetsAndPaymentsLayout
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

data PaymentMethod f = PaymentMethod
  { paymentMethodMoney :: Money f,
    paymentMethodMoneyLabel :: Field Text f,
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
  | Viewer
  | QrCode Screen
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (Binary) via GenericType Screen

unQrCode :: Screen -> Screen
unQrCode = \case
  QrCode sc -> unQrCode sc
  sc -> sc

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

data Asset f = Asset
  { assetPrice :: Money f,
    assetPriceLabel :: Field Text f,
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

newDynamicTitleField :: (MonadIO m) => Text -> m (Field DynamicField Unique)
newDynamicTitleField =
  fmap (& #fieldType .~ FieldTypeTitle)
    . newDynamicField
    . DynamicFieldText

newPasswordField :: (MonadIO m) => Text -> m (Field Text Unique)
newPasswordField output =
  newField FieldTypePassword output id

newCurrency :: (MonadIO m) => CurrencyInfo -> m (Currency Unique)
newCurrency cur =
  Currency
    <$> newTextField mempty
    <*> pure cur
    <*> pure Closed

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

newAsset ::
  ( MonadIO m
  ) =>
  Text ->
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

newPaymentMethod ::
  ( MonadIO m
  ) =>
  CurrencyInfo ->
  Maybe Text ->
  m (PaymentMethod Unique)
newPaymentMethod cur addr0 = do
  lbl <- newTextField $ inspectCurrencyInfo cur <> " total"
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

uniqueToIdentity :: (FunctorB f) => f Unique -> f Identity
uniqueToIdentity =
  bmap $ Identity . uniqueValue

identityToUnique :: (TraversableB f, MonadIO m) => f Identity -> m (f Unique)
identityToUnique =
  btraverse $ newUnique . runIdentity

data FieldType
  = -- Rational
    FieldTypeNumber
  | FieldTypePercent
  | -- Text
    FieldTypeText
  | FieldTypeTitle
  | FieldTypeQrCode
  | FieldTypeHtml
  | FieldTypePassword
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (Binary) via GenericType FieldType

htmlFieldType :: FieldType -> Text
htmlFieldType = \case
  FieldTypeNumber -> "number"
  FieldTypePercent -> "number"
  FieldTypeText -> "text"
  FieldTypeTitle -> "text"
  FieldTypeQrCode -> "text"
  FieldTypeHtml -> "text"
  FieldTypePassword -> "password"

userFieldType :: FieldType -> Text
userFieldType = \case
  FieldTypeNumber -> "Number"
  FieldTypePercent -> "Percent"
  FieldTypeText -> "Text"
  FieldTypeTitle -> "Title"
  FieldTypeQrCode -> "QR code"
  FieldTypeHtml -> "HTML"
  FieldTypePassword -> "Password"

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
    fieldAllowCopy :: Bool,
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
        fieldAllowCopy = True,
        fieldModalState = Closed
      }

data FieldPair a f = FieldPair
  { fieldPairKey :: Field Text f,
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
  (MonadIO m) => Text -> DynamicField -> m (FieldPair DynamicField Unique)
newFieldPair key val =
  FieldPair
    <$> newTextField key
    <*> newDynamicField val

unShareUri ::
  ( MonadThrow m,
    MonadIO m
  ) =>
  URI ->
  m (Maybe (StExt Unique))
unShareUri uri = do
  kKm <- URI.mkQueryKey "k"
  kDoc <- URI.mkQueryKey "d"
  kSc <- URI.mkQueryKey "s"
  kPre <- URI.mkQueryKey "p"
  let qs = URI.uriQuery uri
  case (,,,)
    <$> asumMap (qsGet kDoc) qs
    <*> asumMap (qsGet kKm) qs
    <*> asumMap (qsGet kSc) qs
    <*> asumMap (qsGet kPre) qs of
    Nothing -> pure Nothing
    Just (vDoc, vKm, vSc, vPre) -> do
      bKm <- either throwString pure . B64URL.decode $ encodeUtf8 vKm
      bDoc <- either throwString pure . B64URL.decode $ encodeUtf8 vDoc
      bSc <- either throwString pure . B64URL.decode $ encodeUtf8 vSc
      bPre <- either throwString pure . B64URL.decode $ encodeUtf8 vPre
      km <- either (throwString . thd3) pure $ decodeBinary bKm
      ikm <- newPasswordField mempty
      doc <- either (throwString . thd3) pure $ decodeBinary bDoc
      sc <- either (throwString . thd3) pure $ decodeBinary bSc
      iPre <- either (throwString . thd3) pure $ decodeBinary bPre
      pre <- identityToUnique iPre
      pure
        $ Just
          StExt
            { stExtKm = km,
              stExtIkm = ikm,
              stExtDoc = doc,
              stExtPre = pre,
              stExtScreen = sc
            }

qsGet ::
  URI.RText 'URI.QueryKey ->
  URI.QueryParam ->
  Maybe Prelude.Text
qsGet qk = \case
  URI.QueryParam k x | k == qk -> Just $ URI.unRText x
  _ -> Nothing

stQuery :: (MonadThrow m) => St Identity -> m [URI.QueryParam]
stQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      . Aes.encryptHmac aes
      $ encodeBinary (st ^. #stDoc)
  kKm <- URI.mkQueryKey "k"
  vKm <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      . fromEither
      $ fmap (& #kmIkm .~ Ikm mempty) ekm
  kSc <- URI.mkQueryKey "s"
  vSc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stScreen)
  kPre <- URI.mkQueryKey "p"
  vPre <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stPre)
  pure
    [ URI.QueryParam kDoc vDoc,
      URI.QueryParam kKm vKm,
      URI.QueryParam kSc vSc,
      URI.QueryParam kPre vPre
    ]
  where
    aes :: Aes.SomeAesKey
    aes = Aes.drvSomeAesKey @Aes.Word256 $ fromEither ekm
    ekm :: Either Aes.Km Aes.Km
    ekm =
      case st ^. #stIkm . #fieldOutput of
        ikm | ikm == mempty -> Left (st ^. #stKm)
        ikm -> Right $ (st ^. #stKm) & #kmIkm .~ Ikm (encodeUtf8 ikm)
    encodeText :: (MonadThrow m) => BL.ByteString -> m Prelude.Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stExtQuery :: (MonadThrow m) => StExt Identity -> m [URI.QueryParam]
stExtQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtDoc)
  kKm <- URI.mkQueryKey "k"
  vKm <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtKm)
  kSc <- URI.mkQueryKey "s"
  vSc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtScreen)
  kPre <- URI.mkQueryKey "p"
  vPre <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtPre)
  pure
    [ URI.QueryParam kDoc vDoc,
      URI.QueryParam kKm vKm,
      URI.QueryParam kSc vSc,
      URI.QueryParam kPre vPre
    ]
  where
    encodeText :: (MonadThrow m) => BL.ByteString -> m Prelude.Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stUri :: (MonadThrow m) => Model -> m URI
stUri st = do
  uri <- mkURI $ from @Text @Prelude.Text baseUri
  qxs <- stQuery . uniqueToIdentity $ st ^. #modelState
  pure
    $ uri
      { URI.uriQuery = qxs
      }

stExtUri :: (MonadThrow m) => StExt Unique -> m URI
stExtUri ext = do
  uri <- mkURI $ from @Text @Prelude.Text baseUri
  qxs <- stExtQuery $ uniqueToIdentity ext
  pure
    $ uri
      { URI.uriQuery = qxs
      }

baseUri :: Text
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/currency-converter/" <> vsn <> "/index.html"
#endif

setScreenPure :: Screen -> Model -> Model
setScreenPure sc =
  (& #modelState . #stScreen .~ sc)

setScreenAction :: Screen -> Action
setScreenAction =
  pureUpdate 0 . setScreenPure

setExtScreenAction :: Screen -> Action
setExtScreenAction sc =
  pureUpdate 0 (& #modelState . #stExt . _Just . #stExtScreen .~ sc)

shareLink :: forall a. (From Prelude.Text a) => Screen -> Model -> a
shareLink sc =
  from @Prelude.Text @a
    . either impureThrow URI.render
    . stUri
    . setScreenPure sc

vsn :: Text
vsn =
  from @Prelude.Text @Text
    . T.intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version
