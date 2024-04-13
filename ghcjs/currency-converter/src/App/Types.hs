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
  )
where

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
  { dataModelTopMoney :: MoneyModel,
    dataModelBottomMoney :: MoneyModel,
    dataModelTopOrBottom :: TopOrBottom,
    dataModelPaymentMethods :: [PaymentMethod f],
    dataModelPaymentMethodsInput :: PaymentMethod f,
    dataModelIssuer :: f Text,
    dataModelClient :: f Text
  }
  deriving stock (Generic)

deriving stock instance (Eq (f Text)) => Eq (DataModel f)

deriving stock instance (Ord (f Text)) => Ord (DataModel f)

deriving stock instance (Show (f Text)) => Show (DataModel f)

deriving stock instance (Read (f Text)) => Read (DataModel f)

deriving stock instance (Typeable f, Data (f Text)) => Data (DataModel f)

deriving via
  GenericType (DataModel f)
  instance
    ( Typeable f,
      ToJSON (f Text),
      ToJSON (PaymentMethod f)
    ) =>
    ToJSON (DataModel f)

deriving via
  GenericType (DataModel f)
  instance
    ( Typeable f,
      FromJSON (f Text),
      FromJSON (PaymentMethod f)
    ) =>
    FromJSON (DataModel f)

data Unique a = Unique
  { uniqueUuid :: UUID,
    uniqueData :: a
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data MoneyModel = MoneyModel
  { moneyModelAmount :: AmountModel,
    moneyModelCurrency :: CurrencyModel
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType MoneyModel

data AmountModel = AmountModel
  { amountModelUuid :: UUID,
    amountModelInput :: Text,
    amountModelOutput :: Rational
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType AmountModel

data CurrencyModel = CurrencyModel
  { currencyModelUuid :: UUID,
    currencyModelData :: CurrencyInfo,
    currencyModelOpen :: Bool,
    --
    -- TODO : use Unique Text
    --
    currencyModelSearch :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType CurrencyModel

data PaymentMethod f = PaymentMethod
  { paymentMethodMoney :: MoneyModel,
    paymentMethodAddress :: f Text,
    paymentMethodNotes :: f Text,
    paymentMethodAddressQrCode :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Eq (f Text)) => Eq (PaymentMethod f)

deriving stock instance (Ord (f Text)) => Ord (PaymentMethod f)

deriving stock instance (Show (f Text)) => Show (PaymentMethod f)

deriving stock instance (Read (f Text)) => Read (PaymentMethod f)

deriving stock instance (Typeable f, Data (f Text)) => Data (PaymentMethod f)

data ChanItem a = ChanItem
  { chanItemDelay :: Natural,
    chanItemValue :: a
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data Screen
  = Converter
  | InvoiceEditor
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
  deriving (ToJSON, FromJSON) via GenericType Screen

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)
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

deriving stock instance (Read (f Text)) => Read (AssetModel f)

deriving stock instance (Typeable f, Data (f Text)) => Data (AssetModel f)

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
            modelScreen = Converter,
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
      . #amountModelInput
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelData
      . #dataModelTopMoney
      . #moneyModelAmount
      . #amountModelOutput
      .~ unTagged baseAmt
      & #modelData
      . #dataModelTopMoney
      . #moneyModelCurrency
      . #currencyModelData
      .~ baseCur
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelAmount
      . #amountModelInput
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelAmount
      . #amountModelOutput
      .~ unTagged quoteAmt
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelCurrency
      . #currencyModelData
      .~ quoteCur
      --
      -- InvoiceEditor
      --
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmount
      . #amountModelInput
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmount
      . #amountModelOutput
      .~ unTagged baseAmt
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelCurrency
      . #currencyModelData
      .~ baseCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newMoneyModel :: (MonadIO m) => CurrencyInfo -> m MoneyModel
newMoneyModel cur = do
  amtUuid <- newUuid
  curUuid <- newUuid
  pure
    MoneyModel
      { moneyModelAmount =
          AmountModel
            { amountModelUuid = amtUuid,
              amountModelInput = inspectRatioDef @Text @Integer 0,
              amountModelOutput = 0
            },
        moneyModelCurrency =
          CurrencyModel
            { currencyModelUuid = curUuid,
              currencyModelData = cur,
              currencyModelOpen = False,
              currencyModelSearch = mempty
            }
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
