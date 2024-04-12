module App.Types
  ( Model (..),
    Action (..),
    DataModel (..),
    TextModel (..),
    MoneyModel (..),
    AmountModel (..),
    CurrencyModel (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    AssetModel (..),
    TopOrBottom (..),
    newModel,
    pureUpdate,
    newTextModel,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money
import Functora.Prelude as Prelude
import Functora.Rates
import qualified Material.Snackbar as Snackbar
import Miso hiding (view)

data Model = Model
  { modelHide :: Bool,
    modelData :: DataModel,
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

data DataModel = DataModel
  { dataModelTopMoney :: MoneyModel,
    dataModelBottomMoney :: MoneyModel,
    dataModelTopOrBottom :: TopOrBottom,
    dataModelPaymentMethods :: [PaymentMethod],
    dataModelPaymentMethodsInput :: PaymentMethod,
    dataModelIssuer :: TextModel,
    dataModelClient :: TextModel
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data TextModel = TextModel
  { textModelUuid :: UUID,
    textModelData :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data MoneyModel = MoneyModel
  { moneyModelAmount :: AmountModel,
    moneyModelCurrency :: CurrencyModel
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data AmountModel = AmountModel
  { amountModelUuid :: UUID,
    amountModelInput :: Text,
    amountModelOutput :: Rational
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data CurrencyModel = CurrencyModel
  { currencyModelUuid :: UUID,
    currencyModelData :: CurrencyInfo,
    currencyModelOpen :: Bool,
    --
    -- TODO : use TextModel
    --
    currencyModelSearch :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data PaymentMethod = PaymentMethod
  { paymentMethodMoney :: MoneyModel,
    paymentMethodAddress :: TextModel,
    paymentMethodNotes :: TextModel,
    paymentMethodAddressQrCode :: Bool
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data ChanItem a = ChanItem
  { chanItemDelay :: Natural,
    chanItemValue :: a
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data Screen
  = Converter
  | InvoiceEditor
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data AssetModel = AssetModel
  { assetModelDescription :: TextModel,
    assetModelAmount :: AmountModel,
    assetModelCurrency :: CurrencyModel,
    assetModelRates :: QuotesPerBaseAt
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

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
  issuer <- newTextModel
  client <- newTextModel
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

newTextModel :: (MonadIO m) => m TextModel
newTextModel = do
  uuid <- newUuid
  pure
    TextModel
      { textModelUuid = uuid,
        textModelData = mempty
      }

newPaymentMethod :: (MonadIO m) => CurrencyInfo -> m PaymentMethod
newPaymentMethod cur =
  PaymentMethod
    <$> newMoneyModel cur
    <*> newTextModel
    <*> newTextModel
    <*> pure True
