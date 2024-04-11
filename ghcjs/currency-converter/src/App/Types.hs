module App.Types
  ( Model (..),
    Action (..),
    DataModel (..),
    TextModel (..),
    MoneyModel (..),
    CurrencyInput (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    Asset (..),
    TopOrBottom (..),
    newModel,
    pureUpdate,
    newTextInput,
    inspectMoneyAmount,
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
    textModelValue :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data MoneyModel = MoneyModel
  { moneyModelAmountUuid :: UUID,
    moneyModelAmountInput :: Text,
    moneyModelAmountOutput :: Money (Tags 'Signed |+| 'MoneyAmount),
    moneyModelCurrency :: CurrencyInput
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

data Asset = Asset
  { assetDescription :: TextModel,
    assetCurrency :: CurrencyInput
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data CurrencyInput = CurrencyInput
  { currencyInputUuid :: UUID,
    currencyInputInfo :: CurrencyInfo,
    currencyInputOpen :: Bool,
    --
    -- TODO : use TextModel
    --
    currencyInputSearch :: Text
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
  topMoney <- newModelMoney btc
  bottomMoney <- newModelMoney usd
  paymentMethod <- newPaymentMethod btc
  issuer <- newTextInput
  client <- newTextInput
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
      . #moneyModelAmountInput
      .~ inspectMoneyAmount baseAmt
      & #modelData
      . #dataModelTopMoney
      . #moneyModelAmountOutput
      .~ unTag @'Base baseAmt
      & #modelData
      . #dataModelTopMoney
      . #moneyModelCurrency
      . #currencyInputInfo
      .~ baseCur
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelAmountInput
      .~ inspectMoneyAmount quoteAmt
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelAmountOutput
      .~ unTag @'Quote quoteAmt
      & #modelData
      . #dataModelBottomMoney
      . #moneyModelCurrency
      . #currencyInputInfo
      .~ quoteCur
      --
      -- InvoiceEditor
      --
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmountInput
      .~ inspectMoneyAmount baseAmt
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelAmountOutput
      .~ unTag @'Base baseAmt
      & #modelData
      . #dataModelPaymentMethodsInput
      . #paymentMethodMoney
      . #moneyModelCurrency
      . #currencyInputInfo
      .~ baseCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newModelMoney :: (MonadIO m) => CurrencyInfo -> m MoneyModel
newModelMoney cur = do
  amtUuid <- newUuid
  curUuid <- newUuid
  let zero = Tagged 0 :: Money (Tags 'Signed |+| 'MoneyAmount)
  pure
    MoneyModel
      { moneyModelAmountUuid = amtUuid,
        moneyModelAmountInput = inspectMoneyAmount zero,
        moneyModelAmountOutput = zero,
        moneyModelCurrency =
          CurrencyInput
            { currencyInputUuid = curUuid,
              currencyInputInfo = cur,
              currencyInputOpen = False,
              currencyInputSearch = mempty
            }
      }

--
-- NOTE : In most cases we don't need JSM.
--
pureUpdate :: Natural -> (Model -> Model) -> Action
pureUpdate delay =
  PushUpdate (pure ())
    . ChanItem delay

newTextInput :: (MonadIO m) => m TextModel
newTextInput = do
  uuid <- newUuid
  pure
    TextModel
      { textModelUuid = uuid,
        textModelValue = mempty
      }

newPaymentMethod :: (MonadIO m) => CurrencyInfo -> m PaymentMethod
newPaymentMethod cur =
  PaymentMethod
    <$> newModelMoney cur
    <*> newTextInput
    <*> newTextInput
    <*> pure True

inspectMoneyAmount :: (MoneyTags tags, From String a) => Money tags -> a
inspectMoneyAmount =
  inspectRatio defaultRatioFormat . unTagged
