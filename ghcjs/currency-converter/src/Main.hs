{-# LANGUAGE CPP #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JS
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Ws
import qualified Data.ByteString.Lazy as BL
#endif
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Version as Version
import Functora.Money
import Functora.Prelude as Prelude
import Functora.Rates
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.Snackbar as Snackbar
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import qualified Miso
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Paths_app as Paths
import qualified Text.Fuzzy as Fuzzy

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app = do
  js0 <- BL.readFile "static/app.js"
  js1 <- BL.readFile "static/material-components-web.min.js"
  js2 <- BL.readFile "static/material-components-web-elm.min.js"
  Warp.runSettings
    ( Warp.setPort
        8080
        (Warp.setTimeout 3600 Warp.defaultSettings)
    )
    =<< JS.jsaddleOr
      Ws.defaultConnectionOptions
      (app >> syncPoint)
      (router $ js0 <> js1 <> js2)
  where
    router js req =
      case Wai.pathInfo req of
        ("static" : _) -> staticApp (defaultWebAppSettings ".") req
        ("site.webmanifest" : _) -> staticApp (defaultWebAppSettings "static") req
        _ -> JS.jsaddleAppWithJs (JS.jsaddleJs False <> js) req
#else
runApp :: IO () -> IO ()
runApp = id
#endif

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

data PaymentMethod = PaymentMethod
  { paymentMethodMoney :: ModelMoney,
    paymentMethodReference :: Text,
    paymentMethodNotes :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data Screen
  = Converter
  | InvoiceEditor
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data ChanItem a = ChanItem
  { chanItemDelay :: Natural,
    chanItemValue :: a
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

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

data ModelData = ModelData
  { modelDataTopMoney :: ModelMoney,
    modelDataBottomMoney :: ModelMoney,
    modelDataTopOrBottom :: TopOrBottom,
    modelDataPaymentMethods :: [PaymentMethod],
    modelDataPaymentMethodsInput :: PaymentMethod,
    modelDataUserName :: Text
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

data Action
  = Noop
  | InitUpdate
  | TimeUpdate
  | ChanUpdate Model
  | PushUpdate (JSM ()) (ChanItem (Model -> Model))

--
-- NOTE : In most cases we don't need JSM.
--
pureUpdate :: Natural -> (Model -> Model) -> Action
pureUpdate delay =
  PushUpdate (pure ())
    . ChanItem delay

pushActionQueue :: (MonadIO m) => Model -> ChanItem (Model -> Model) -> m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)

extendedEvents :: Map MisoString Bool
extendedEvents =
  defaultEvents
    & Map.insert "MDCDialog:close" True
    & Map.insert "MDCDrawer:close" True
    & Map.insert "MDCList:action" True
    & Map.insert "MDCSnackbar:closed" True
    & Map.insert "MDCTab:interacted" True
    & Map.insert "MDCSlider:input" True
    & Map.insert "MDCMenuSurface:close" True
    & Map.insert "MDCChip:interaction" True
    & Map.insert "MDCIconButtonToggle:change" True

main :: IO ()
main = do
  st <- newModel
  runApp
    $ startApp
      App
        { model = st,
          update = updateModel,
          Miso.view = viewModel,
          subs = mempty,
          events = extendedEvents,
          initialAction = InitUpdate,
          mountPoint = Nothing, -- defaults to 'body'
          logLevel = Off
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel Noop st = noEff st
updateModel InitUpdate prevSt = do
  batchEff
    prevSt
    [ do
        sleepSeconds 60
        pure TimeUpdate,
      do
        --
        -- NOTE : making a new pair of TChans to avoid deadlocks
        -- when running in ghcid mode and reloading page without
        -- restarting executable. It's ok, the overhead is low.
        --
        prod <- liftIO newBroadcastTChanIO
        cons <- liftIO . atomically $ dupTChan prod
        let nextSt =
              prevSt
                { modelProducerQueue = prod,
                  modelConsumerQueue = cons
                }
        pushActionQueue nextSt $ ChanItem 0 (& #modelHide .~ False)
        pure $ ChanUpdate nextSt
    ]
updateModel TimeUpdate st = do
  batchEff
    st
    [ do
        sleepSeconds 60
        pure TimeUpdate,
      do
        ct <- getCurrentTime
        unless (upToDate ct $ st ^. #modelUpdatedAt)
          . pushActionQueue st
          $ ChanItem 0 id
        pure Noop
    ]
updateModel (ChanUpdate prevSt) _ = do
  batchEff
    prevSt
    [ do
        syncInputs prevSt
        pure Noop,
      do
        actions <- drainTChan $ prevSt ^. #modelConsumerQueue
        nextSt <- foldlM (\acc updater -> evalModel $ updater acc) prevSt actions
        pure $ ChanUpdate nextSt
    ]
updateModel (PushUpdate runJSM updater) st = do
  batchEff
    st
    [ do
        pushActionQueue st updater
        pure Noop,
      do
        runJSM
        pure Noop
    ]

drainTChan :: (MonadIO m) => TChan (ChanItem a) -> m [a]
drainTChan chan = do
  item <- liftIO . atomically $ readTChan chan
  liftIO
    . fmap ((chanItemValue item :) . reverse)
    . drainInto []
    $ chanItemDelay item
  where
    drainInto acc delay = do
      item <- atomically $ tryReadTChan chan
      case item of
        Nothing | delay == 0 -> pure acc
        Nothing -> do
          sleepMilliSeconds $ from @Natural @Integer delay
          drainInto acc 0
        Just next ->
          drainInto (chanItemValue next : acc)
            . max delay
            $ chanItemDelay next

syncInputs :: Model -> JSM ()
syncInputs st =
  --
  -- NOTE : The "correct" way is to use "controlled input" with
  -- TextField.setValue but it does cause race conditions
  -- when user types "too fast":
  --
  -- https://github.com/dmjio/miso/issues/272
  --
  forM_ enumerate $ \loc -> do
    let moneyLens = getMoneyOptic loc
    JS.eval @Text
      $ "var el = document.getElementById('"
      <> htmlUuid (st ^. cloneLens moneyLens . #modelMoneyAmountUuid)
      <> "'); if (el && !(el.getElementsByTagName('input')[0] === document.activeElement)) el.value = '"
      <> (st ^. cloneLens moneyLens . #modelMoneyAmountInput)
      <> "';"

copyIntoClipboard :: (Show a, Data a) => Model -> a -> JSM ()
copyIntoClipboard st x = do
  let txt = inspect @Text x
  unless (null txt) $ do
    clip <- JS.global JS.! ("navigator" :: Text) JS.! ("clipboard" :: Text)
    prom <- clip ^. JS.js1 ("writeText" :: Text) txt
    success <- JS.function $ \_ _ _ -> push $ "Copied " <> txt
    failure <- JS.function $ \_ _ _ -> push $ "Failed to copy " <> txt
    void $ prom ^. JS.js2 ("then" :: Text) success failure
  where
    push =
      pushActionQueue st
        . updateSnackbar Snackbar.clearQueue

updateSnackbar ::
  ( Show a,
    Data a
  ) =>
  (Snackbar.Queue Action -> Snackbar.Queue Action) ->
  a ->
  ChanItem (Model -> Model)
updateSnackbar before x =
  ChanItem 0 (& #modelSnackbarQueue %~ (Snackbar.addMessage msg . before))
  where
    msg =
      inspect x
        & Snackbar.message
        & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
        & Snackbar.setOnActionIconClick snackbarClosed

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel st = do
  let loc = st ^. #modelData . #modelDataTopOrBottom
  let baseLens = getBaseMoneyOptic loc
  let quoteLens = getQuoteMoneyOptic loc
  let baseAmtInput =
        case st ^. cloneLens baseLens . #modelMoneyAmountInput of
          amt
            | null amt ->
                inspectMoneyAmount
                  $ st
                  ^. cloneLens baseLens
                  . #modelMoneyAmountOutput
          amt -> amt
  baseAmtResult <-
    tryAny $ parseMoney baseAmtInput
  case baseAmtResult of
    Left {} -> pure st
    Right baseAmt ->
      withMarket (st ^. #modelMarket) $ do
        let funds =
              Funds
                baseAmt
                $ st
                ^. cloneLens baseLens
                . #modelMoneyCurrencyInfo
                . #currencyInfoCode
        quote <-
          getQuote funds
            $ st
            ^. cloneLens quoteLens
            . #modelMoneyCurrencyInfo
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        ct <- getCurrentTime
        pure
          $ st
          & cloneLens baseLens
          . #modelMoneyAmountInput
          .~ baseAmtInput
          & cloneLens baseLens
          . #modelMoneyAmountOutput
          .~ unTag @'Base baseAmt
          & cloneLens quoteLens
          . #modelMoneyAmountInput
          .~ inspectMoneyAmount quoteAmt
          & cloneLens quoteLens
          . #modelMoneyAmountOutput
          .~ unTag @'Quote quoteAmt
          & #modelUpdatedAt
          .~ ct

getMoneyOptic :: TopOrBottom -> ALens' Model ModelMoney
getMoneyOptic = \case
  Top -> #modelData . #modelDataTopMoney
  Bottom -> #modelData . #modelDataBottomMoney

getBaseMoneyOptic :: TopOrBottom -> ALens' Model ModelMoney
getBaseMoneyOptic = \case
  Top -> #modelData . #modelDataTopMoney
  Bottom -> #modelData . #modelDataBottomMoney

getQuoteMoneyOptic :: TopOrBottom -> ALens' Model ModelMoney
getQuoteMoneyOptic = \case
  Top -> #modelData . #modelDataBottomMoney
  Bottom -> #modelData . #modelDataTopMoney

viewModel :: Model -> View Action
#ifndef __GHCJS__
viewModel st =
  div_
    mempty
    [ link_ [rel_ "stylesheet", href_ "static/material-components-web.min.css"],
      link_ [rel_ "stylesheet", href_ "static/material-icons.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      mainWidget st
    ]
#else
viewModel st =
  mainWidget st
#endif

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    $ [ LayoutGrid.inner
          ( [ class_ "container"
            ]
              --
              -- NOTE : Hiding widget on the first render to avoid flickering.
              --
              <> ( if st ^. #modelHide
                    then [style_ [("display", "none")]]
                    else mempty
                 )
          )
          ( screenWidget st
              <> [
                   -- LayoutGrid.cell [LayoutGrid.span12]
                   --   . (: mempty)
                   --   $ div_ mempty [inspect $ st ^. #modelData],
                   swapScreenWidget st,
                   copyright,
                   Snackbar.snackbar (Snackbar.config snackbarClosed)
                    $ modelSnackbarQueue st
                 ]
          )
      ]
    <> ( if st ^. #modelHide
          then [div_ [class_ "lds-dual-ring"] mempty]
          else mempty
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelScreen = Converter} =
  [ amountWidget st Top,
    currencyWidget st $ getMoneyOptic Top,
    amountWidget st Bottom,
    currencyWidget st $ getMoneyOptic Bottom,
    swapAmountsWidget,
    swapCurrenciesWidget
  ]
screenWidget st@Model {modelScreen = InvoiceEditor} =
  [ amountWidget st Top,
    currencyWidget st $ getMoneyOptic Top,
    currencyWidget st
      $ #modelData
      . #modelDataPaymentMethodsInput
      . #paymentMethodMoney
  ]

amountWidget :: Model -> TopOrBottom -> View Action
amountWidget st loc =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ TextField.outlined
        $ TextField.config
        & TextField.setType (Just "number")
        & TextField.setOnInput onInputAction
        & TextField.setLeadingIcon
          ( Just
              $ TextField.icon
                [ class_ "mdc-text-field__icon--leading",
                  intProp "tabindex" 0,
                  textProp "role" "button",
                  onClick onCopyAction
                ]
                "content_copy"
          )
        & TextField.setTrailingIcon
          ( Just
              $ TextField.icon
                [ class_ "mdc-text-field__icon--trailing",
                  intProp "tabindex" 0,
                  textProp "role" "button",
                  onClick onClearAction
                ]
                "close"
          )
        & TextField.setPlaceholder
          ( Just
              . inspectCurrencyInfo
              $ st
              ^. cloneLens moneyLens
              . #modelMoneyCurrencyInfo
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_
              . ms
              . htmlUuid @Text
              $ st
              ^. cloneLens moneyLens
              . #modelMoneyAmountUuid,
            onKeyDown $ onKeyDownAction uuid,
            onBlur onBlurAction
          ]
    ]
  where
    moneyLens = getMoneyOptic loc
    uuid = st ^. cloneLens moneyLens . #modelMoneyAmountUuid
    input = st ^. cloneLens moneyLens . #modelMoneyAmountInput
    output = st ^. cloneLens moneyLens . #modelMoneyAmountOutput
    valid =
      (parseMoney input == Just output)
        || (input == inspectMoneyAmount output)
    onBlurAction =
      pureUpdate 300 $ \st' ->
        if valid
          then st'
          else
            st'
              & cloneLens moneyLens
              . #modelMoneyAmountInput
              .~ inspectMoneyAmount output
    onInputAction txt =
      pureUpdate 300 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyAmountInput
          .~ from @String @Text txt
          & #modelData
          . #modelDataTopOrBottom
          .~ loc
    onCopyAction =
      PushUpdate
        ( copyIntoClipboard st
            $ st
            ^. cloneLens moneyLens
            . #modelMoneyAmountInput
        )
        ( ChanItem 0 id
        )
    onClearAction =
      PushUpdate
        ( do
            focus . ms $ htmlUuid @Text uuid
            void
              . JS.eval @Text
              $ "var el = document.getElementById('"
              <> htmlUuid uuid
              <> "'); if (el) el.value = '';"
        )
        ( ChanItem 300 $ \st' ->
            st'
              & cloneLens moneyLens
              . #modelMoneyAmountInput
              .~ mempty
              & #modelData
              . #modelDataTopOrBottom
              .~ loc
        )

onKeyDownAction :: UUID -> KeyCode -> Action
onKeyDownAction uuid (KeyCode code) =
  let enterOrEscape = [13, 27] :: [Int]
   in PushUpdate
        ( when (code `elem` enterOrEscape)
            . void
            . JS.eval @Text
            $ "document.getElementById('"
            <> htmlUuid uuid
            <> "').getElementsByTagName('input')[0].blur();"
        )
        ( ChanItem 300 id
        )

currencyWidget ::
  Model ->
  ALens' Model ModelMoney ->
  View Action
currencyWidget st moneyLens =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
    ]
    [ Button.raised
        ( Button.setOnClick opened
            . Button.setAttributes
              [ class_ "fill"
              ]
            $ Button.config
        )
        . inspectCurrencyInfo
        $ st
        ^. cloneLens moneyLens
        . #modelMoneyCurrencyInfo,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. cloneLens moneyLens
                  . #modelMoneyCurrencyOpen
              )
        )
        ( Dialog.dialogContent
            Nothing
            [ currencyListWidget st moneyLens
            ]
            . (: mempty)
            $ div_
              [ class_ "fill"
              ]
              [ TextField.outlined
                  . TextField.setType (Just "text")
                  . ( if st
                        ^. cloneLens moneyLens
                        . #modelMoneyCurrencyOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. cloneLens moneyLens
                                . #modelMoneyCurrencySearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. cloneLens moneyLens
                        . #modelMoneyCurrencyInfo
                    )
                  . TextField.setAttributes
                    [ class_ "fill",
                      id_ . ms $ htmlUuid @Text uuid,
                      onKeyDown $ onKeyDownAction uuid
                    ]
                  $ TextField.config,
                Button.raised
                  ( Button.config
                      & Button.setOnClick closed
                      & Button.setAttributes
                        [ class_ "fill"
                        ]
                  )
                  "Cancel"
              ]
        )
    ]
  where
    uuid = st ^. cloneLens moneyLens . #modelMoneyCurrencyUuid
    search input =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyCurrencySearch
          .~ from @String @Text input
    opened =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyCurrencyOpen
          .~ True
          & cloneLens moneyLens
          . #modelMoneyCurrencySearch
          .~ mempty
    closed =
      pureUpdate 0 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyCurrencyOpen
          .~ False
          & cloneLens moneyLens
          . #modelMoneyCurrencySearch
          .~ mempty

currencyListWidget ::
  Model ->
  ALens' Model ModelMoney ->
  View Action
currencyListWidget st moneyLens =
  List.list
    List.config
    ( currencyListItemWidget moneyLens current
        $ maybe current NonEmpty.head matching
    )
    . fmap (currencyListItemWidget moneyLens current)
    $ maybe mempty NonEmpty.tail matching
  where
    currencies = st ^. #modelCurrencies
    current = st ^. cloneLens moneyLens . #modelMoneyCurrencyInfo
    search = st ^. cloneLens moneyLens . #modelMoneyCurrencySearch
    matching =
      nonEmpty
        . fmap Fuzzy.original
        $ Fuzzy.filter
          search
          (toList currencies)
          "<"
          ">"
          inspectCurrencyInfo
          False

currencyListItemWidget ::
  ALens' Model ModelMoney ->
  CurrencyInfo ->
  CurrencyInfo ->
  ListItem.ListItem Action
currencyListItemWidget moneyLens current item =
  ListItem.listItem
    ( ListItem.config
        & ListItem.setSelected
          ( if current == item
              then Just ListItem.activated
              else Nothing
          )
        & ListItem.setOnClick
          ( pureUpdate 0 $ \st ->
              st
                & cloneLens moneyLens
                . #modelMoneyCurrencyOpen
                .~ False
                & cloneLens moneyLens
                . #modelMoneyCurrencySearch
                .~ mempty
                & cloneLens moneyLens
                . #modelMoneyCurrencyInfo
                .~ item
          )
    )
    [ Miso.text . toMisoString $ inspectCurrencyInfo @Text item
    ]

swapAmountsWidget :: View Action
swapAmountsWidget =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    . (: mempty)
    $ Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
      "Swap amounts"
  where
    onClickAction =
      pureUpdate 0 $ \st ->
        let baseInput =
              st ^. #modelData . #modelDataTopMoney . #modelMoneyAmountInput
            baseOutput =
              st ^. #modelData . #modelDataTopMoney . #modelMoneyAmountOutput
            quoteInput =
              st ^. #modelData . #modelDataBottomMoney . #modelMoneyAmountInput
            quoteOutput =
              st ^. #modelData . #modelDataBottomMoney . #modelMoneyAmountOutput
         in st
              & #modelData
              . #modelDataTopMoney
              . #modelMoneyAmountInput
              .~ quoteInput
              & #modelData
              . #modelDataTopMoney
              . #modelMoneyAmountOutput
              .~ quoteOutput
              & #modelData
              . #modelDataBottomMoney
              . #modelMoneyAmountInput
              .~ baseInput
              & #modelData
              . #modelDataBottomMoney
              . #modelMoneyAmountOutput
              .~ baseOutput
              & #modelData
              . #modelDataTopOrBottom
              .~ Top

swapCurrenciesWidget :: View Action
swapCurrenciesWidget =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    . (: mempty)
    $ Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
      "Swap currencies"
  where
    onClickAction =
      pureUpdate 0 $ \st ->
        let baseCurrency =
              st ^. #modelData . #modelDataTopMoney . #modelMoneyCurrencyInfo
            quoteCurrency =
              st ^. #modelData . #modelDataBottomMoney . #modelMoneyCurrencyInfo
         in st
              & #modelData
              . #modelDataTopMoney
              . #modelMoneyCurrencyInfo
              .~ quoteCurrency
              & #modelData
              . #modelDataBottomMoney
              . #modelMoneyCurrencyInfo
              .~ baseCurrency
              & #modelData
              . #modelDataTopOrBottom
              .~ Top

swapScreenWidget :: Model -> View Action
swapScreenWidget st =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span8Tablet,
      LayoutGrid.span4Phone
    ]
    . (: mempty)
    . Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
    $ case st ^. #modelScreen of
      Converter -> "Create invoice"
      InvoiceEditor -> "Show converter"
  where
    onClickAction =
      PushUpdate
        ( do
            --
            -- NOTE : Need to sync text inputs on new screen.
            --
            sleepMilliSeconds 300
            pushActionQueue st $ ChanItem 0 id
        )
        $ ChanItem
          0
          ( &
              #modelScreen
                %~ ( \case
                      Converter -> InvoiceEditor
                      InvoiceEditor -> Converter
                   )
          )

copyright :: View Action
copyright =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.subtitle2,
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text "\169 2024 Functora. All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text ". ",
      Miso.text . ms $ "Version " <> vsn <> "."
    ]

snackbarClosed :: Snackbar.MessageId -> Action
snackbarClosed msg =
  pureUpdate 0 (& #modelSnackbarQueue %~ Snackbar.close msg)

inspectMoneyAmount :: (MoneyTags tags, From String a) => Money tags -> a
inspectMoneyAmount =
  inspectRatio defaultRatioFormat . unTagged

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs

vsn :: Text
vsn =
  T.intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version
