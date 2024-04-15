{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Unique (..),
    Action (..),
    St (..),
    MoneyModel (..),
    AmountModel (..),
    CurrencyModel (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    AssetModel (..),
    TopOrBottom (..),
    newUnique,
    newModel,
    pureUpdate,
    newStateModelUnique,
    newStateModelIdentity,
  )
where

import Data.Functor.Barbie
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Cfg
import Functora.Money
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
  | PushUpdate (JSM ()) (ChanItem (Model -> Model))

type Std f =
  ( Typeable f,
    Eq (f Text),
    Eq (f AmountModel),
    Eq (f CurrencyModel),
    Ord (f Text),
    Ord (f AmountModel),
    Ord (f CurrencyModel),
    Show (f Text),
    Show (f AmountModel),
    Show (f CurrencyModel),
    Data (f Text),
    Data (f AmountModel),
    Data (f CurrencyModel)
  )

data St f = St
  { stateTopMoney :: MoneyModel f,
    stateBottomMoney :: MoneyModel f,
    stateTopOrBottom :: TopOrBottom,
    statePaymentMethods :: [PaymentMethod f],
    statePaymentMethodsInput :: PaymentMethod f,
    stateIssuer :: f Text,
    stateClient :: f Text
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

data MoneyModel f = MoneyModel
  { moneyModelAmount :: f AmountModel,
    moneyModelCurrency :: f CurrencyModel
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (MoneyModel f)

deriving stock instance (Std f) => Ord (MoneyModel f)

deriving stock instance (Std f) => Show (MoneyModel f)

deriving stock instance (Std f) => Data (MoneyModel f)

instance FunctorB MoneyModel

instance TraversableB MoneyModel

deriving via
  GenericType (MoneyModel Identity)
  instance
    ToJSON (MoneyModel Identity)

deriving via
  GenericType (MoneyModel Identity)
  instance
    FromJSON (MoneyModel Identity)

data AmountModel = AmountModel
  { amountModelInput :: Text,
    amountModelOutput :: Rational
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType AmountModel

data CurrencyModel = CurrencyModel
  { currencyModelValue :: CurrencyInfo,
    currencyModelOpen :: Bool,
    --
    -- TODO : use Unique Text??
    --
    currencyModelSearch :: Text
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType CurrencyModel

data PaymentMethod f = PaymentMethod
  { paymentMethodMoney :: MoneyModel f,
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
  | InvoiceEditor
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType Screen

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Enum, Bounded, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType TopOrBottom

data AssetModel f = AssetModel
  { assetModelDescription :: f Text,
    assetModelAmount :: AmountModel,
    assetModelCurrency :: CurrencyModel,
    assetModelRates :: QuotesPerBaseAt
  }
  deriving stock (Generic)

deriving stock instance (Std f) => Eq (AssetModel f)

deriving stock instance (Std f) => Ord (AssetModel f)

deriving stock instance (Std f) => Show (AssetModel f)

deriving stock instance (Std f) => Data (AssetModel f)

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
  topMoney <- newMoneyModel btc
  bottomMoney <- newMoneyModel usd
  paymentMethod <- newPaymentMethod btc
  issuer <- newUnique mempty
  client <- newUnique mempty
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
                  stateIssuer = issuer,
                  stateClient = client
                },
            modelScreen = InvoiceEditor,
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
    let baseAmt = Tagged 1 :: Money (Tags 'Signed |+| 'Base |+| 'MoneyAmount)
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
      . #moneyModelAmount
      . #uniqueValue
      . #amountModelInput
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #stateTopMoney
      . #moneyModelAmount
      . #uniqueValue
      . #amountModelOutput
      .~ unTagged baseAmt
      & #modelState
      . #stateTopMoney
      . #moneyModelCurrency
      . #uniqueValue
      . #currencyModelValue
      .~ baseCur
      & #modelState
      . #stateBottomMoney
      . #moneyModelAmount
      . #uniqueValue
      . #amountModelInput
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelState
      . #stateBottomMoney
      . #moneyModelAmount
      . #uniqueValue
      . #amountModelOutput
      .~ unTagged quoteAmt
      & #modelState
      . #stateBottomMoney
      . #moneyModelCurrency
      . #uniqueValue
      . #currencyModelValue
      .~ quoteCur
      --
      -- InvoiceEditor
      --
      & #modelState
      . #statePaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmount
      . #uniqueValue
      . #amountModelInput
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #statePaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmount
      . #uniqueValue
      . #amountModelOutput
      .~ unTagged baseAmt
      & #modelState
      . #statePaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelCurrency
      . #uniqueValue
      . #currencyModelValue
      .~ baseCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newMoneyModel :: (MonadIO m) => CurrencyInfo -> m (MoneyModel Unique)
newMoneyModel curInfo = do
  amt <-
    newUnique
      AmountModel
        { amountModelInput = inspectRatioDef @Text @Integer 0,
          amountModelOutput = 0
        }
  cur <-
    newUnique
      CurrencyModel
        { currencyModelValue = curInfo,
          currencyModelOpen = False,
          currencyModelSearch = mempty
        }
  pure
    MoneyModel
      { moneyModelAmount = amt,
        moneyModelCurrency = cur
      }

--
-- NOTE : In most cases we don't need JSM.
--
pureUpdate :: Natural -> (Model -> Model) -> Action
pureUpdate delay =
  PushUpdate (pure ())
    . ChanItem delay

newUnique :: (MonadIO m) => a -> m (Unique a)
newUnique x =
  Unique
    <$> newUuid
    <*> pure x

newPaymentMethod :: (MonadIO m) => CurrencyInfo -> m (PaymentMethod Unique)
newPaymentMethod cur =
  PaymentMethod
    <$> newMoneyModel cur
    <*> newUnique mempty
    <*> newUnique mempty
    <*> pure True

newStateModelIdentity :: St Unique -> St Identity
newStateModelIdentity =
  bmap (Identity . uniqueValue)

newStateModelUnique ::
  ( MonadIO m
  ) =>
  St Identity ->
  m (St Unique)
newStateModelUnique =
  btraverse (newUnique . runIdentity)
