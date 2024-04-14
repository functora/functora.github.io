{-# LANGUAGE UndecidableInstances #-}

module App.Types
  ( Model (..),
    Action (..),
    DataModel (..),
    MoneyModel (..),
    AmountModel (..),
    CurrencyModel (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    AssetModel (..),
    TopOrBottom (..),
    Unique (..),
    newUnique,
    newModel,
    pureUpdate,
    newDataModelUnique,
    newDataModelIdentity,
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
    modelData :: DataModel Unique,
    modelScreen :: Screen,
    modelMarket :: MVar Market,
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelSnackbarQueue :: Snackbar.Queue Action,
    modelProducerQueue :: TChan (ChanItem (Model -> Model)),
    modelConsumerQueue :: TChan (ChanItem (Model -> Model)),
    modelUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Generic)

data Action
  = Noop
  | InitUpdate
  | TimeUpdate
  | ChanUpdate Model
  | PushUpdate (JSM ()) (ChanItem (Model -> Model))

data DataModel f = DataModel
  { dataModelTopMoney :: MoneyModel f,
    dataModelBottomMoney :: MoneyModel f,
    dataModelTopOrBottom :: TopOrBottom,
    dataModelPaymentMethods :: [PaymentMethod f],
    dataModelPaymentMethodsInput :: PaymentMethod f,
    dataModelIssuer :: f Text,
    dataModelClient :: f Text
  }
  deriving stock (Generic)

deriving stock instance
  ( Eq (f Text),
    Eq (f AmountModel),
    Eq (f CurrencyModel)
  ) =>
  Eq (DataModel f)

deriving stock instance
  ( Ord (f Text),
    Ord (f AmountModel),
    Ord (f CurrencyModel)
  ) =>
  Ord (DataModel f)

deriving stock instance
  ( Show (f Text),
    Show (f AmountModel),
    Show (f CurrencyModel)
  ) =>
  Show (DataModel f)

deriving stock instance
  ( Typeable f,
    Data (f Text),
    Data (f AmountModel),
    Data (f CurrencyModel)
  ) =>
  Data (DataModel f)

instance FunctorB DataModel

instance TraversableB DataModel

deriving via
  GenericType (DataModel f)
  instance
    ( Typeable f,
      ToJSON (f Text),
      ToJSON (MoneyModel f),
      ToJSON (PaymentMethod f)
    ) =>
    ToJSON (DataModel f)

deriving via
  GenericType (DataModel f)
  instance
    ( Typeable f,
      FromJSON (f Text),
      FromJSON (MoneyModel f),
      FromJSON (PaymentMethod f)
    ) =>
    FromJSON (DataModel f)

data Unique a = Unique
  { uniqueUuid :: UUID,
    uniqueData :: a
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

data MoneyModel f = MoneyModel
  { moneyModelAmount :: f AmountModel,
    moneyModelCurrency :: f CurrencyModel
  }
  deriving stock (Generic)

deriving stock instance
  ( Eq (f Text),
    Eq (f AmountModel),
    Eq (f CurrencyModel)
  ) =>
  Eq (MoneyModel f)

deriving stock instance
  ( Ord (f Text),
    Ord (f AmountModel),
    Ord (f CurrencyModel)
  ) =>
  Ord (MoneyModel f)

deriving stock instance
  ( Show (f Text),
    Show (f AmountModel),
    Show (f CurrencyModel)
  ) =>
  Show (MoneyModel f)

deriving stock instance
  ( Typeable f,
    Data (f Text),
    Data (f AmountModel),
    Data (f CurrencyModel)
  ) =>
  Data (MoneyModel f)

instance FunctorB MoneyModel

instance TraversableB MoneyModel

deriving via
  GenericType (MoneyModel f)
  instance
    ( Typeable f,
      ToJSON (f AmountModel),
      ToJSON (f CurrencyModel)
    ) =>
    ToJSON (MoneyModel f)

deriving via
  GenericType (MoneyModel f)
  instance
    ( Typeable f,
      FromJSON (f AmountModel),
      FromJSON (f CurrencyModel)
    ) =>
    FromJSON (MoneyModel f)

data AmountModel = AmountModel
  { amountModelInput :: Text,
    amountModelOutput :: Rational
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType AmountModel

data CurrencyModel = CurrencyModel
  { currencyModelData :: CurrencyInfo,
    currencyModelOpen :: Bool,
    --
    -- TODO : use Unique Text
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

deriving stock instance
  ( Eq (f Text),
    Eq (f AmountModel),
    Eq (f CurrencyModel)
  ) =>
  Eq (PaymentMethod f)

deriving stock instance
  ( Ord (f Text),
    Ord (f AmountModel),
    Ord (f CurrencyModel)
  ) =>
  Ord (PaymentMethod f)

deriving stock instance
  ( Show (f Text),
    Show (f AmountModel),
    Show (f CurrencyModel)
  ) =>
  Show (PaymentMethod f)

deriving stock instance
  ( Typeable f,
    Data (f Text),
    Data (f AmountModel),
    Data (f CurrencyModel)
  ) =>
  Data (PaymentMethod f)

instance FunctorB PaymentMethod

instance TraversableB PaymentMethod

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

deriving stock instance (Eq (f Text)) => Eq (AssetModel f)

deriving stock instance (Ord (f Text)) => Ord (AssetModel f)

deriving stock instance (Show (f Text)) => Show (AssetModel f)

deriving stock instance (Data (f Text), Typeable f) => Data (AssetModel f)

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
            modelData =
              DataModel
                { dataModelTopMoney = topMoney,
                  dataModelBottomMoney = bottomMoney,
                  dataModelTopOrBottom = Top,
                  dataModelPaymentMethods = mempty,
                  dataModelPaymentMethodsInput = paymentMethod,
                  dataModelIssuer = issuer,
                  dataModelClient = client
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
      & #modelData
      . #dataModelTopMoney
      . #moneyModelAmount
      . #uniqueData
      . #amountModelInput
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelData
      . #dataModelTopMoney
      . #moneyModelAmount
      . #uniqueData
      . #amountModelOutput
      .~ unTagged baseAmt
      & #modelData
      . #dataModelTopMoney
      . #moneyModelCurrency
      . #uniqueData
      . #currencyModelData
      .~ baseCur
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelAmount
      . #uniqueData
      . #amountModelInput
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelAmount
      . #uniqueData
      . #amountModelOutput
      .~ unTagged quoteAmt
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelCurrency
      . #uniqueData
      . #currencyModelData
      .~ quoteCur
      --
      -- InvoiceEditor
      --
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmount
      . #uniqueData
      . #amountModelInput
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmount
      . #uniqueData
      . #amountModelOutput
      .~ unTagged baseAmt
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelCurrency
      . #uniqueData
      . #currencyModelData
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
        { currencyModelData = curInfo,
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

newDataModelIdentity :: DataModel Unique -> DataModel Identity
newDataModelIdentity = bmap (Identity . uniqueData)

newDataModelUnique :: (MonadIO m) => DataModel Identity -> m (DataModel Unique)
newDataModelUnique = btraverse (newUnique . runIdentity)
