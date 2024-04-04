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
import Miso.String hiding (cons, foldl, null, reverse)
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
        _ -> JS.jsaddleAppWithJs (JS.jsaddleJs False <> js) req
#else
runApp :: IO () -> IO ()
runApp = id
#endif

data Model = Model
  { modelHide :: Bool,
    modelData :: ModelData,
    modelMarket :: MVar Market,
    modelCurrencies :: NonEmpty CurrencyInfo,
    modelSnackbarQueue :: Snackbar.Queue Action,
    modelProducerQueue :: TChan (Model -> Model),
    modelConsumerQueue :: TChan (Model -> Model),
    modelUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Generic)

data TopOrBottom
  = Top
  | Bottom
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data ModelMoney = ModelMoney
  { modelMoneyAmountInput :: Text,
    modelMoneyAmountOutput :: Money (Tags 'Signed |+| 'MoneyAmount),
    modelMoneyCurrencyInfo :: CurrencyInfo,
    modelMoneyCurrencyOpen :: Bool,
    modelMoneyCurrencySearch :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data ModelData = ModelData
  { modelDataTopMoney :: ModelMoney,
    modelDataBottomMoney :: ModelMoney,
    modelDataTopOrBottom :: TopOrBottom
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

mkModel :: (MonadThrow m, MonadUnliftIO m) => m Model
mkModel = do
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
  let zero = Tagged 0 :: Money (Tags 'Signed |+| 'MoneyAmount)
  let final =
        ModelData
          { modelDataTopMoney =
              ModelMoney
                { modelMoneyAmountInput = inspectMoneyAmount zero,
                  modelMoneyAmountOutput = zero,
                  modelMoneyCurrencyInfo = btc,
                  modelMoneyCurrencyOpen = False,
                  modelMoneyCurrencySearch = mempty
                },
            modelDataBottomMoney =
              ModelMoney
                { modelMoneyAmountInput = inspectMoneyAmount zero,
                  modelMoneyAmountOutput = zero,
                  modelMoneyCurrencyInfo = usd,
                  modelMoneyCurrencyOpen = False,
                  modelMoneyCurrencySearch = mempty
                },
            modelDataTopOrBottom = Top
          }
  let st =
        Model
          { modelHide = True,
            modelData = final,
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
    let final' =
          ModelData
            { modelDataTopMoney =
                ModelMoney
                  { modelMoneyAmountInput = inspectMoneyAmount baseAmt,
                    modelMoneyAmountOutput = unTag @'Base baseAmt,
                    modelMoneyCurrencyInfo = baseCur,
                    modelMoneyCurrencyOpen = False,
                    modelMoneyCurrencySearch = mempty
                  },
              modelDataBottomMoney =
                ModelMoney
                  { modelMoneyAmountInput = inspectMoneyAmount quoteAmt,
                    modelMoneyAmountOutput = unTag @'Quote quoteAmt,
                    modelMoneyCurrencyInfo = quoteCur,
                    modelMoneyCurrencyOpen = False,
                    modelMoneyCurrencySearch = mempty
                  },
              modelDataTopOrBottom = Top
            }
    pure
      Model
        { modelHide = True,
          modelData = final',
          modelMarket = market,
          modelCurrencies = currenciesInfo,
          modelSnackbarQueue = Snackbar.initialQueue,
          modelProducerQueue = prod,
          modelConsumerQueue = cons,
          modelUpdatedAt = ct
        }

data Action
  = Noop
  | InitUpdate
  | TimeUpdate
  | ChanUpdate Model
  | PushUpdate (JSM ()) (Model -> Model)

--
-- NOTE : In most cases we don't need JSM.
--
pureUpdate :: (Model -> Model) -> Action
pureUpdate = PushUpdate $ pure ()

pushActionQueue :: (MonadIO m) => Model -> (Model -> Model) -> m ()
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
  st <- mkModel
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
        pushActionQueue nextSt (& #modelHide .~ False)
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
        unless (upToDate ct $ st ^. #modelUpdatedAt) $ pushActionQueue st id
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

drainTChan :: (MonadIO m) => TChan a -> m [a]
drainTChan chan = do
  item <- liftIO . atomically $ readTChan chan
  liftIO
    . fmap ((item :) . reverse)
    $ drainInto [] False
  where
    drainInto acc debounced = do
      item <- atomically $ tryReadTChan chan
      case item of
        Nothing | debounced -> pure acc
        Nothing -> sleepMilliSeconds 300 >> drainInto acc True
        Just next -> drainInto (next : acc) False

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
    JS.eval @Text
      $ "var el = document.getElementById('"
      <> inspect loc
      <> "'); if (el && !(el.getElementsByTagName('input')[0] === document.activeElement)) el.value = '"
      <> (st ^. #modelData . getMoneyOptic loc . #modelMoneyAmountInput)
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
  Model ->
  Model
updateSnackbar before x =
  (& #modelSnackbarQueue %~ (Snackbar.addMessage msg . before))
  where
    msg =
      inspect x
        & Snackbar.message
        & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
        & Snackbar.setOnActionIconClick snackbarClosed

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel st = do
  let loc = st ^. #modelData . #modelDataTopOrBottom
  let baseAmtInput =
        case st ^. #modelData . getBaseMoneyOptic loc . #modelMoneyAmountInput of
          amt
            | null amt ->
                inspectMoneyAmount
                  $ st
                  ^. #modelData
                  . getBaseMoneyOptic loc
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
                ^. #modelData
                . getBaseMoneyOptic loc
                . #modelMoneyCurrencyInfo
                . #currencyInfoCode
        quote <-
          getQuote funds
            $ st
            ^. #modelData
            . getQuoteMoneyOptic loc
            . #modelMoneyCurrencyInfo
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        ct <- getCurrentTime
        pure
          $ st
          & #modelData
          . getBaseMoneyOptic loc
          . #modelMoneyAmountInput
          .~ baseAmtInput
          & #modelData
          . getBaseMoneyOptic loc
          . #modelMoneyAmountOutput
          .~ unTag @'Base baseAmt
          & #modelData
          . getQuoteMoneyOptic loc
          . #modelMoneyAmountInput
          .~ inspectMoneyAmount quoteAmt
          & #modelData
          . getQuoteMoneyOptic loc
          . #modelMoneyAmountOutput
          .~ unTag @'Quote quoteAmt
          & #modelUpdatedAt
          .~ ct

getMoneyOptic :: TopOrBottom -> Lens' ModelData ModelMoney
getMoneyOptic = \case
  Top -> #modelDataTopMoney
  Bottom -> #modelDataBottomMoney

getBaseMoneyOptic :: TopOrBottom -> Lens' ModelData ModelMoney
getBaseMoneyOptic = \case
  Top -> #modelDataTopMoney
  Bottom -> #modelDataBottomMoney

getQuoteMoneyOptic :: TopOrBottom -> Lens' ModelData ModelMoney
getQuoteMoneyOptic = \case
  Top -> #modelDataBottomMoney
  Bottom -> #modelDataTopMoney

viewModel :: Model -> View Action
viewModel st =
  div_
    mempty
    [ link_ [rel_ "stylesheet", href_ "static/material-components-web.min.css"],
      link_ [rel_ "stylesheet", href_ "static/material-icons.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      mainWidget st
    ]

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
          [ amountWidget st Top,
            currencyWidget st Top,
            amountWidget st Bottom,
            currencyWidget st Bottom,
            swapAmountsWidget,
            swapCurrenciesWidget,
            -- LayoutGrid.cell [LayoutGrid.span12]
            --   . (: mempty)
            --   $ div_ mempty [inspect $ st ^. #modelData],
            copyright,
            Snackbar.snackbar (Snackbar.config snackbarClosed)
              $ modelSnackbarQueue st
          ]
      ]
    <> ( if st ^. #modelHide
          then [div_ [class_ "lds-dual-ring"] mempty]
          else mempty
       )

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
        & TextField.setValid valid
        & TextField.setOnInput onInputAction
        & TextField.setRequired True
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
              ^. #modelData
              . getMoneyOptic loc
              . #modelMoneyCurrencyInfo
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_ $ inspect loc,
            onKeyDown onKeyDownAction,
            onBlur onBlurAction
          ]
    ]
  where
    input = st ^. #modelData . getMoneyOptic loc . #modelMoneyAmountInput
    output = st ^. #modelData . getMoneyOptic loc . #modelMoneyAmountOutput
    valid =
      (parseMoney input == Just output)
        || (input == inspectMoneyAmount output)
    onBlurAction =
      pureUpdate $ \st' ->
        if valid
          then st'
          else
            st'
              & #modelData
              . getMoneyOptic loc
              . #modelMoneyAmountInput
              .~ inspectMoneyAmount output
    onKeyDownAction (KeyCode code) =
      let enterOrEscape = [13, 27] :: [Int]
       in PushUpdate
            ( when (code `elem` enterOrEscape)
                . void
                . JS.eval @Text
                $ "document.getElementById('"
                <> inspect loc
                <> "').getElementsByTagName('input')[0].blur();"
            )
            id
    onInputAction txt =
      pureUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic loc
          . #modelMoneyAmountInput
          .~ from @String @Text txt
          & #modelData
          . #modelDataTopOrBottom
          .~ loc
    onCopyAction =
      PushUpdate
        ( copyIntoClipboard st
            $ st
            ^. #modelData
            . getMoneyOptic loc
            . #modelMoneyAmountInput
        )
        id
    onClearAction =
      PushUpdate
        ( do
            focus $ inspect loc
            void
              . JS.eval @Text
              $ "var el = document.getElementById('"
              <> inspect loc
              <> "'); if (el) el.value = '';"
        )
        ( \st' ->
            st'
              & #modelData
              . getMoneyOptic loc
              . #modelMoneyAmountInput
              .~ mempty
              & #modelData
              . #modelDataTopOrBottom
              .~ loc
        )

currencyWidget ::
  Model ->
  TopOrBottom ->
  View Action
currencyWidget st loc =
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
        ^. #modelData
        . getMoneyOptic loc
        . #modelMoneyCurrencyInfo,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. #modelData
                  . getMoneyOptic loc
                  . #modelMoneyCurrencyOpen
              )
        )
        ( Dialog.dialogContent
            Nothing
            [ currencyListWidget st loc
            ]
            . (: mempty)
            $ div_
              [ class_ "fill"
              ]
              [ TextField.outlined
                  . TextField.setType (Just "text")
                  . ( if st
                        ^. #modelData
                        . getMoneyOptic loc
                        . #modelMoneyCurrencyOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. #modelData
                                . getMoneyOptic loc
                                . #modelMoneyCurrencySearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. #modelData
                        . getMoneyOptic loc
                        . #modelMoneyCurrencyInfo
                    )
                  . TextField.setAttributes
                    [ class_ "fill"
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
    search input =
      pureUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic loc
          . #modelMoneyCurrencySearch
          .~ from @String @Text input
    opened =
      pureUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic loc
          . #modelMoneyCurrencyOpen
          .~ True
          & #modelData
          . getMoneyOptic loc
          . #modelMoneyCurrencySearch
          .~ mempty
    closed =
      pureUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic loc
          . #modelMoneyCurrencyOpen
          .~ False
          & #modelData
          . getMoneyOptic loc
          . #modelMoneyCurrencySearch
          .~ mempty

currencyListWidget :: Model -> TopOrBottom -> View Action
currencyListWidget st loc =
  List.list
    List.config
    ( currencyListItemWidget loc current
        $ maybe current NonEmpty.head matching
    )
    . fmap (currencyListItemWidget loc current)
    $ maybe mempty NonEmpty.tail matching
  where
    currencies = st ^. #modelCurrencies
    current = st ^. #modelData . getMoneyOptic loc . #modelMoneyCurrencyInfo
    search = st ^. #modelData . getMoneyOptic loc . #modelMoneyCurrencySearch
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
  TopOrBottom ->
  CurrencyInfo ->
  CurrencyInfo ->
  ListItem.ListItem Action
currencyListItemWidget loc current item =
  ListItem.listItem
    ( ListItem.config
        & ListItem.setSelected
          ( if current == item
              then Just ListItem.activated
              else Nothing
          )
        & ListItem.setOnClick
          ( pureUpdate $ \st ->
              st
                & #modelData
                . getMoneyOptic loc
                . #modelMoneyCurrencyOpen
                .~ False
                & #modelData
                . getMoneyOptic loc
                . #modelMoneyCurrencySearch
                .~ mempty
                & #modelData
                . getMoneyOptic loc
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
      pureUpdate $ \st ->
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
      pureUpdate $ \st ->
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
      a_ [href_ "license.html"] [Miso.text "Terms and Conditions"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text "."
    ]

snackbarClosed :: Snackbar.MessageId -> Action
snackbarClosed msg =
  pureUpdate (& #modelSnackbarQueue %~ Snackbar.close msg)

inspectMoneyAmount :: (MoneyTags tags, From String a) => Money tags -> a
inspectMoneyAmount =
  inspectRatio defaultRatioFormat . unTagged

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs
