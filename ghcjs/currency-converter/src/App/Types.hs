{-# LANGUAGE CPP #-}

module App.Types
  ( Model (..),
    Action (..),
    ModelData (..),
    ModelMoney (..),
    PaymentMethod (..),
    ChanItem (..),
    Screen (..),
    TopOrBottom (..),
    newModel,
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
    modelData :: ModelData,
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

data ModelData = ModelData
  { modelDataTopMoney :: ModelMoney,
    modelDataBottomMoney :: ModelMoney,
    modelDataTopOrBottom :: TopOrBottom,
    modelDataPaymentMethods :: [PaymentMethod],
    modelDataPaymentMethodsInput :: PaymentMethod,
    modelDataUserName :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data ModelMoney = ModelMoney
  { modelMoneyAmountUuid :: UUID,
    modelMoneyAmountInput :: Text,
    modelMoneyAmountOutput :: Money (Tags 'Signed |+| 'MoneyAmount),
    modelMoneyCurrencyUuid :: UUID,
    modelMoneyCurrencyInfo :: CurrencyInfo,
    modelMoneyCurrencyOpen :: Bool,
    modelMoneyCurrencySearch :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data PaymentMethod = PaymentMethod
  { paymentMethodMoney :: ModelMoney,
    paymentMethodReference :: Text,
    paymentMethodNotes :: Text
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
  methodMoney <- newModelMoney btc
  let st =
        Model
          { modelHide = True,
            modelData =
              ModelData
                { modelDataTopMoney = topMoney,
                  modelDataBottomMoney = bottomMoney,
                  modelDataTopOrBottom = Top,
                  modelDataPaymentMethods = mempty,
                  modelDataPaymentMethodsInput =
                    PaymentMethod
                      { paymentMethodMoney = methodMoney,
                        paymentMethodReference = mempty,
                        paymentMethodNotes = mempty
                      },
                  modelDataUserName = mempty
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
      . #modelDataTopMoney
      . #modelMoneyAmountInput
      .~ inspectMoneyAmount baseAmt
      & #modelData
      . #modelDataTopMoney
      . #modelMoneyAmountOutput
      .~ unTag @'Base baseAmt
      & #modelData
      . #modelDataTopMoney
      . #modelMoneyCurrencyInfo
      .~ baseCur
      & #modelData
      . #modelDataBottomMoney
      . #modelMoneyAmountInput
      .~ inspectMoneyAmount quoteAmt
      & #modelData
      . #modelDataBottomMoney
      . #modelMoneyAmountOutput
      .~ unTag @'Quote quoteAmt
      & #modelData
      . #modelDataBottomMoney
      . #modelMoneyCurrencyInfo
      .~ quoteCur
      --
      -- InvoiceEditor
      --
      & #modelData
      . #modelDataPaymentMethodsInput
      . #paymentMethodMoney
      . #modelMoneyAmountInput
      .~ inspectMoneyAmount baseAmt
      & #modelData
      . #modelDataPaymentMethodsInput
      . #paymentMethodMoney
      . #modelMoneyAmountOutput
      .~ unTag @'Base baseAmt
      & #modelData
      . #modelDataPaymentMethodsInput
      . #paymentMethodMoney
      . #modelMoneyCurrencyInfo
      .~ baseCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newModelMoney :: (MonadIO m) => CurrencyInfo -> m ModelMoney
newModelMoney cur = do
  amtUuid <- newUuid
  curUuid <- newUuid
  let zero = Tagged 0 :: Money (Tags 'Signed |+| 'MoneyAmount)
  pure
    ModelMoney
      { modelMoneyAmountUuid = amtUuid,
        modelMoneyAmountInput = inspectMoneyAmount zero,
        modelMoneyAmountOutput = zero,
        modelMoneyCurrencyUuid = curUuid,
        modelMoneyCurrencyInfo = cur,
        modelMoneyCurrencyOpen = False,
        modelMoneyCurrencySearch = mempty
      }

inspectMoneyAmount :: (MoneyTags tags, From String a) => Money tags -> a
inspectMoneyAmount =
  inspectRatio defaultRatioFormat . unTagged
