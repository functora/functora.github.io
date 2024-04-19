{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Unique (..),
    Action (..),
    Std,
    St (..),
    Money (..),
    Amount (..),
    Currency (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    Asset (..),
    TopOrBottom (..),
    newModel,
    newUnique,
    pureUpdate,
    newUniqueState,
    newIdentityState,
    TextProp (..),
    newTextProp,
    dupTextProp,
  )
where

import Data.Functor.Barbie
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Cfg
import Functora.Money hiding (Currency, Money)
import qualified Functora.Money as Money
import Functora.Prelude as Prelude
import Functora.Rates
import qualified Material.Snackbar as Snackbar
import Miso hiding (view)

data Model = Model
  { modelHide :: Bool,
    modelState :: St Unique,
    modelScreen :: Screen,
    modelMarket :: MVar Market,
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelSnackbarQueue :: Snackbar.Queue Action,
    modelProducerQueue :: TChan (ChanItem (Model -> Model)),
    modelConsumerQueue :: TChan (ChanItem (Model -> Model)),
    modelUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Generic)

data Unique a = Unique
  { uniqueUuid :: UUID,
    uniqueValue :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data Action
  = Noop
  | InitUpdate
  | TimeUpdate
  | ChanUpdate Model
  | PushUpdate (JSM (ChanItem (Model -> Model)))

type Std f =
  ( Typeable f,
    Eq (f Text),
    Ord (f Text),
    Show (f Text),
    Data (f Text)
  )

data St f = St
  { stateTopMoney :: Money f,
    stateBottomMoney :: Money f,
    stateTopOrBottom :: TopOrBottom,
    statePaymentMethods :: [PaymentMethod f],
    statePaymentMethodsInput :: PaymentMethod f,
    stateTextProps :: [TextProp f],
    stateAssets :: [Asset f]
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (St f)

deriving stock instance (Std f) => Ord (St f)

deriving stock instance (Std f) => Show (St f)

deriving stock instance (Std f) => Data (St f)

instance FunctorB St

instance TraversableB St

deriving via GenericType (St Identity) instance ToJSON (St Identity)

deriving via GenericType (St Identity) instance FromJSON (St Identity)

data Money f = Money
  { moneyAmount :: Amount f,
    moneyCurrency :: Currency f
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (Money f)

deriving stock instance (Std f) => Ord (Money f)

deriving stock instance (Std f) => Show (Money f)

deriving stock instance (Std f) => Data (Money f)

instance FunctorB Money

instance TraversableB Money

deriving via GenericType (Money Identity) instance ToJSON (Money Identity)

deriving via GenericType (Money Identity) instance FromJSON (Money Identity)

data Amount f = Amount
  { amountInput :: f Text,
    amountOutput :: Rational
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (Amount f)

deriving stock instance (Std f) => Ord (Amount f)

deriving stock instance (Std f) => Show (Amount f)

deriving stock instance (Std f) => Data (Amount f)

instance FunctorB Amount

instance TraversableB Amount

deriving via GenericType (Amount Identity) instance ToJSON (Amount Identity)

deriving via GenericType (Amount Identity) instance FromJSON (Amount Identity)

data Currency f = Currency
  { currencyInput :: f Text,
    currencyOutput :: CurrencyInfo,
    currencyOpen :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (Currency f)

deriving stock instance (Std f) => Ord (Currency f)

deriving stock instance (Std f) => Show (Currency f)

deriving stock instance (Std f) => Data (Currency f)

instance FunctorB Currency

instance TraversableB Currency

deriving via GenericType (Currency Identity) instance ToJSON (Currency Identity)

deriving via
  GenericType (Currency Identity)
  instance
    FromJSON (Currency Identity)

data PaymentMethod f = PaymentMethod
  { paymentMethodMoney :: Money f,
    paymentMethodAddress :: f Text,
    paymentMethodNotes :: f Text,
    paymentMethodAddressQrCode :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (PaymentMethod f)

deriving stock instance (Std f) => Ord (PaymentMethod f)

deriving stock instance (Std f) => Show (PaymentMethod f)

deriving stock instance (Std f) => Data (PaymentMethod f)

instance FunctorB PaymentMethod

instance TraversableB PaymentMethod

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
  | DocumentEditor
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType Screen

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType TopOrBottom

data Asset f = Asset
  { assetProps :: [TextProp f],
    assetMoney :: Money f
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (Asset f)

deriving stock instance (Std f) => Ord (Asset f)

deriving stock instance (Std f) => Show (Asset f)

deriving stock instance (Std f) => Data (Asset f)

instance FunctorB Asset

instance TraversableB Asset

deriving via GenericType (Asset Identity) instance ToJSON (Asset Identity)

deriving via GenericType (Asset Identity) instance FromJSON (Asset Identity)

newModel :: (MonadThrow m, MonadUnliftIO m) => m Model
newModel = do
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
  topMoney <- newMoney btc
  bottomMoney <- newMoney usd
  paymentMethod <- newPaymentMethod btc
  issuer <- newTextProp "Issuer" mempty
  client <- newTextProp "Client" mempty
  asset <- newAsset usd
  let st =
        Model
          { modelHide = True,
            modelState =
              St
                { stateTopMoney = topMoney,
                  stateBottomMoney = bottomMoney,
                  stateTopOrBottom = Top,
                  statePaymentMethods = mempty,
                  statePaymentMethodsInput = paymentMethod,
                  stateTextProps = [issuer, client],
                  stateAssets = [asset]
                },
            modelScreen = DocumentEditor,
            modelMarket = market,
            modelCurrencies = [btc, usd],
            modelSnackbarQueue = Snackbar.initialQueue,
            modelProducerQueue = prod,
            modelConsumerQueue = cons,
            modelUpdatedAt = ct
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
      . #amountInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #stateTopMoney
      . #moneyAmount
      . #amountOutput
      .~ unTagged baseAmt
      & #modelState
      . #stateTopMoney
      . #moneyCurrency
      . #currencyOutput
      .~ baseCur
      & #modelState
      . #stateBottomMoney
      . #moneyAmount
      . #amountInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelState
      . #stateBottomMoney
      . #moneyAmount
      . #amountOutput
      .~ unTagged quoteAmt
      & #modelState
      . #stateBottomMoney
      . #moneyCurrency
      . #currencyOutput
      .~ quoteCur
      --
      -- DocumentEditor
      --
      & #modelState
      . #statePaymentMethodsInput
      . #paymentMethodMoney
      . #moneyAmount
      . #amountInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #statePaymentMethodsInput
      . #paymentMethodMoney
      . #moneyAmount
      . #amountOutput
      .~ unTagged baseAmt
      & #modelState
      . #statePaymentMethodsInput
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
      . #assetMoney
      . #moneyAmount
      . #amountInput
      . #uniqueValue
      .~ inspectRatioDef (100 :: Rational)
      & #modelState
      . #stateAssets
      . each
      . #assetMoney
      . #moneyAmount
      . #amountOutput
      .~ 100
      & #modelState
      . #stateAssets
      . each
      . #assetMoney
      . #moneyCurrency
      . #currencyOutput
      .~ quoteCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newAmount :: (MonadIO m) => m (Amount Unique)
newAmount =
  Amount
    <$> newUnique (inspectRatioDef @Text @Integer 0)
    <*> pure 0

newCurrency :: (MonadIO m) => CurrencyInfo -> m (Currency Unique)
newCurrency cur =
  Currency
    <$> newUnique mempty
    <*> pure cur
    <*> pure False

newMoney :: (MonadIO m) => CurrencyInfo -> m (Money Unique)
newMoney cur =
  Money
    <$> newAmount
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
    <$> newUuid
    <*> pure x

newPaymentMethod :: (MonadIO m) => CurrencyInfo -> m (PaymentMethod Unique)
newPaymentMethod cur =
  PaymentMethod
    <$> newMoney cur
    <*> newUnique mempty
    <*> newUnique mempty
    <*> pure True

newAsset :: (MonadIO m) => CurrencyInfo -> m (Asset Unique)
newAsset cur =
  Asset
    <$> fmap (: mempty) (newTextProp mempty mempty)
    <*> newMoney cur

newIdentityState :: St Unique -> St Identity
newIdentityState =
  bmap (Identity . uniqueValue)

newUniqueState :: (MonadIO m) => St Identity -> m (St Unique)
newUniqueState =
  btraverse (newUnique . runIdentity)

data TextProp f = TextProp
  { textPropKey :: f Text,
    textPropValue :: f Text,
    textPropValueQrCode :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (TextProp f)

deriving stock instance (Std f) => Ord (TextProp f)

deriving stock instance (Std f) => Show (TextProp f)

deriving stock instance (Std f) => Data (TextProp f)

instance FunctorB TextProp

instance TraversableB TextProp

deriving via
  GenericType (TextProp Identity)
  instance
    ToJSON (TextProp Identity)

deriving via
  GenericType (TextProp Identity)
  instance
    FromJSON (TextProp Identity)

newTextProp :: (MonadIO m) => Text -> Text -> m (TextProp Unique)
newTextProp key val =
  TextProp
    <$> newUnique key
    <*> newUnique val
    <*> pure False

dupTextProp :: (MonadIO m) => m (TextProp Unique -> TextProp Unique)
dupTextProp = do
  keyUuid <- newUuid
  valUuid <- newUuid
  pure $ \this ->
    this
      & #textPropKey
      . #uniqueUuid
      .~ keyUuid
      & #textPropValue
      . #uniqueUuid
      .~ valUuid
