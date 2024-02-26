{-# LANGUAGE CPP #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
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
import Functora.Rates hiding (Quote)
import qualified Language.Javascript.JSaddle.Evaluate as JSaddle
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.List as List
import qualified Material.List.Item as ListItem
import qualified Material.Snackbar as Snackbar
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import Miso hiding (view)
import qualified Miso
import Miso.String
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Fuzzy as Fuzzy

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app = do
  elm <- BL.readFile "static/material-components-web-elm.min.js"
  Warp.runSettings
    ( Warp.setPort
        8080
        (Warp.setTimeout 3600 Warp.defaultSettings)
    )
    =<< JSaddle.jsaddleOr
      Ws.defaultConnectionOptions
      (app >> syncPoint)
      (router elm)
  where
    router elm req =
      case Wai.pathInfo req of
        ("static" : _) -> staticApp (defaultWebAppSettings ".") req
        _ -> JSaddle.jsaddleAppWithJs (JSaddle.jsaddleJs False <> elm) req
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
    modelUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Generic)

data BaseOrQuote
  = Base
  | Quote
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data ModelMoney = ModelMoney
  { modelMoneyAmountInput :: Text,
    modelMoneyAmountOutput :: Money Rational,
    modelMoneyAmountActive :: Bool,
    modelMoneyCurrencyInfo :: CurrencyInfo,
    modelMoneyCurrencyOpen :: Bool,
    modelMoneyCurrencySearch :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

data ModelData = ModelData
  { -- TODO : use timestamed data
    modelDataBaseMoney :: ModelMoney,
    modelDataQuoteMoney :: ModelMoney,
    modelDataBaseOrQuote :: BaseOrQuote
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

mkModel :: (MonadThrow m, MonadUnliftIO m) => m Model
mkModel = do
  ct <- getCurrentTime
  market <- mkMarket
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
  let zero = Money 0 :: Money Rational
  let final =
        ModelData
          { modelDataBaseMoney =
              ModelMoney
                { modelMoneyAmountInput = inspectMoneyAmount zero,
                  modelMoneyAmountOutput = zero,
                  modelMoneyAmountActive = False,
                  modelMoneyCurrencyInfo = btc,
                  modelMoneyCurrencyOpen = False,
                  modelMoneyCurrencySearch = mempty
                },
            modelDataQuoteMoney =
              ModelMoney
                { modelMoneyAmountInput = inspectMoneyAmount zero,
                  modelMoneyAmountOutput = zero,
                  modelMoneyAmountActive = False,
                  modelMoneyCurrencyInfo = usd,
                  modelMoneyCurrencyOpen = False,
                  modelMoneyCurrencySearch = mempty
                },
            modelDataBaseOrQuote = Base
          }
  let st =
        Model
          { modelHide = True,
            modelData = final,
            modelMarket = market,
            modelCurrencies = [btc, usd],
            modelSnackbarQueue = Snackbar.initialQueue,
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
    let baseAmt = Money 1 :: Money Rational
    quote <-
      getQuote
        (Funds baseAmt $ currencyInfoCode baseCur)
        $ currencyInfoCode quoteCur
    let quoteAmt = quoteMoneyAmount quote
    let final' =
          ModelData
            { modelDataBaseMoney =
                ModelMoney
                  { modelMoneyAmountInput = inspectMoneyAmount baseAmt,
                    modelMoneyAmountOutput = baseAmt,
                    modelMoneyAmountActive = False,
                    modelMoneyCurrencyInfo = baseCur,
                    modelMoneyCurrencyOpen = False,
                    modelMoneyCurrencySearch = mempty
                  },
              modelDataQuoteMoney =
                ModelMoney
                  { modelMoneyAmountInput = inspectMoneyAmount quoteAmt,
                    modelMoneyAmountOutput = quoteAmt,
                    modelMoneyAmountActive = False,
                    modelMoneyCurrencyInfo = quoteCur,
                    modelMoneyCurrencyOpen = False,
                    modelMoneyCurrencySearch = mempty
                  },
              modelDataBaseOrQuote = Base
            }
    pure
      Model
        { modelHide = True,
          modelData = final',
          modelMarket = market,
          modelCurrencies = currenciesInfo,
          modelSnackbarQueue = Snackbar.initialQueue,
          modelUpdatedAt = ct
        }

data Action
  = Noop
  | LoopModelUpdate
  | PureModelUpdate (Model -> Model)
  | EvalModelUpdate (Model -> Model)

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
          initialAction = LoopModelUpdate,
          mountPoint = Nothing, -- defaults to 'body'
          logLevel = Off
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel Noop st = noEff st
updateModel LoopModelUpdate st =
  batchEff
    st
    [ do
        --
        -- NOTE : The "correct" way is to use "controlled input" with
        -- TextField.setValue but without proper Action queue synchronization
        -- or mutex it does cause race conditions when user types "too fast":
        --
        -- https://github.com/dmjio/miso/issues/272
        --
        forM_ enumerate $ \boq ->
          unless (st ^. #modelData . getMoneyOptic boq . #modelMoneyAmountActive)
            . void
            . JSaddle.eval @Text
            $ "var el = document.getElementById('"
            <> inspect boq
            <> "'); if (el) el.value = '"
            <> (st ^. #modelData . getMoneyOptic boq . #modelMoneyAmountInput)
            <> "';"
        sleepMilliSeconds 300
        pure LoopModelUpdate,
      if st ^. #modelHide
        then pure $ PureModelUpdate (& #modelHide .~ False)
        else do
          ct <- getCurrentTime
          if upToDate ct $ st ^. #modelUpdatedAt
            then pure Noop
            else PureModelUpdate <$> evalModel st
    ]
updateModel (PureModelUpdate updater) st = noEff $ updater st
updateModel (EvalModelUpdate updater) prevSt = do
  let nextSt = updater prevSt
  --
  -- NOTE : The "correct" way is to emit PureModelUpdate effect instead,
  -- but without proper Action queue synchronization or mutex it does cause
  -- race conditions when user types "too fast". Impure evalModel function
  -- is cached and fast anyways. It's ok.
  --
  -- TODO : experiment with implementing proper Action queue.
  --
  let res = Unsafe.unsafePerformIO . tryAny $ evalModel nextSt
  case res of
    Left e -> do
      let msg =
            inspect e
              & Snackbar.message
              & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
              & Snackbar.setOnActionIconClick snackbarClosed
      noEff $ nextSt & #modelSnackbarQueue %~ Snackbar.addMessage msg
    Right next ->
      noEff $ next nextSt

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m (Model -> Model)
evalModel st = do
  let boq = st ^. #modelData . #modelDataBaseOrQuote
  baseAmtResult <-
    tryAny
      . parseMoney
      $ st
      ^. #modelData
      . getBaseMoneyOptic boq
      . #modelMoneyAmountInput
  case baseAmtResult of
    Left {} -> pure id
    Right baseAmt ->
      withMarket (st ^. #modelMarket) $ do
        let funds =
              Funds
                { fundsMoneyAmount = baseAmt,
                  fundsCurrencyCode =
                    st
                      ^. #modelData
                      . getBaseMoneyOptic boq
                      . #modelMoneyCurrencyInfo
                      . #currencyInfoCode
                }
        quote <-
          getQuote funds
            $ st
            ^. #modelData
            . getQuoteMoneyOptic boq
            . #modelMoneyCurrencyInfo
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        ct <- getCurrentTime
        pure $ \st' ->
          st'
            & #modelData
            . getBaseMoneyOptic boq
            . #modelMoneyAmountOutput
            .~ baseAmt
            & #modelData
            . getQuoteMoneyOptic boq
            . #modelMoneyAmountInput
            .~ inspectMoneyAmount quoteAmt
            & #modelData
            . getQuoteMoneyOptic boq
            . #modelMoneyAmountOutput
            .~ quoteAmt
            & #modelUpdatedAt
            .~ ct

getMoneyOptic :: BaseOrQuote -> Lens' ModelData ModelMoney
getMoneyOptic = \case
  Base -> #modelDataBaseMoney
  Quote -> #modelDataQuoteMoney

getBaseMoneyOptic :: BaseOrQuote -> Lens' ModelData ModelMoney
getBaseMoneyOptic = \case
  Base -> #modelDataBaseMoney
  Quote -> #modelDataQuoteMoney

getQuoteMoneyOptic :: BaseOrQuote -> Lens' ModelData ModelMoney
getQuoteMoneyOptic = \case
  Base -> #modelDataQuoteMoney
  Quote -> #modelDataBaseMoney

viewModel :: Model -> View Action
viewModel st =
  div_
    mempty
    [ link_
        [ rel_ "stylesheet",
          href_
            "https://unpkg.com/material-components-web@6.0.0/dist/material-components-web.min.css"
        ],
      link_
        [ rel_ "stylesheet",
          href_ "https://fonts.googleapis.com/icon?family=Material+Icons"
        ],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      mainWidget st,
      script_ [src_ "static/clipboard.min.js"] mempty,
      script_
        [ src_
            "https://unpkg.com/material-components-web@6.0.0/dist/material-components-web.min.js",
          defer_ "defer"
        ]
        mempty,
      script_ [src_ "static/app.js", defer_ "defer"] mempty
    ]

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    ( [ LayoutGrid.alignMiddle
      ]
        --
        -- NOTE : need to hide widget on the first render to avoid flickering
        --
        <> ( if st ^. #modelHide
              then [style_ [("display", "none")]]
              else mempty
           )
    )
    [ LayoutGrid.inner
        [ class_ "container"
        ]
        [ amountWidget st Base,
          currencyWidget st Base,
          amountWidget st Quote,
          currencyWidget st Quote,
          swapAmountsWidget,
          swapCurrenciesWidget,
          -- LayoutGrid.cell [LayoutGrid.span12]
          --   . (: mempty)
          --   $ div_ mempty [inspect $ st ^. #modelData],
          Snackbar.snackbar (Snackbar.config snackbarClosed)
            $ modelSnackbarQueue st
        ]
    ]

amountWidget :: Model -> BaseOrQuote -> View Action
amountWidget st boq =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
    ]
    . (: mempty)
    . TextField.outlined
    $ TextField.config
    & TextField.setType (Just "number")
    & TextField.setValid valid
    & TextField.setOnInput onInputAction
    & TextField.setPlaceholder
      ( Just
          . inspectCurrencyInfo
          $ st
          ^. #modelData
          . getMoneyOptic boq
          . #modelMoneyCurrencyInfo
      )
    & TextField.setRequired True
    & TextField.setAttributes
      [ class_ "fill",
        id_ $ inspect boq,
        onBlur onBlurAction
      ]
  where
    input = st ^. #modelData . getMoneyOptic boq . #modelMoneyAmountInput
    output = st ^. #modelData . getMoneyOptic boq . #modelMoneyAmountOutput
    valid =
      (parseMoney input == Just output)
        || (input == inspectMoneyAmount output)
    onBlurAction =
      EvalModelUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyAmountActive
          .~ False
    onInputAction txt =
      EvalModelUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyAmountInput
          .~ from @String @Text txt
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyAmountActive
          .~ True
          & #modelData
          . #modelDataBaseOrQuote
          .~ boq

currencyWidget ::
  Model ->
  BaseOrQuote ->
  View Action
currencyWidget st boq =
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
        . getMoneyOptic boq
        . #modelMoneyCurrencyInfo,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. #modelData
                  . getMoneyOptic boq
                  . #modelMoneyCurrencyOpen
              )
        )
        ( Dialog.dialogContent
            Nothing
            [ currencyListWidget st boq
            ]
            . (: mempty)
            $ div_
              [ class_ "fill"
              ]
              [ TextField.outlined
                  . TextField.setType (Just "text")
                  . ( if st
                        ^. #modelData
                        . getMoneyOptic boq
                        . #modelMoneyCurrencyOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. #modelData
                                . getMoneyOptic boq
                                . #modelMoneyCurrencySearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. #modelData
                        . getMoneyOptic boq
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
      EvalModelUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyCurrencySearch
          .~ from @String @Text input
    opened =
      EvalModelUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyCurrencyOpen
          .~ True
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyCurrencySearch
          .~ mempty
    closed =
      EvalModelUpdate $ \st' ->
        st'
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyCurrencyOpen
          .~ False
          & #modelData
          . getMoneyOptic boq
          . #modelMoneyCurrencySearch
          .~ mempty

currencyListWidget :: Model -> BaseOrQuote -> View Action
currencyListWidget st boq =
  List.list
    List.config
    ( currencyListItemWidget boq current
        $ maybe current NonEmpty.head matching
    )
    . fmap (currencyListItemWidget boq current)
    $ maybe mempty NonEmpty.tail matching
  where
    currencies = st ^. #modelCurrencies
    current = st ^. #modelData . getMoneyOptic boq . #modelMoneyCurrencyInfo
    search = st ^. #modelData . getMoneyOptic boq . #modelMoneyCurrencySearch
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
  BaseOrQuote ->
  CurrencyInfo ->
  CurrencyInfo ->
  ListItem.ListItem Action
currencyListItemWidget boq current item =
  ListItem.listItem
    ( ListItem.config
        & ListItem.setSelected
          ( if current == item
              then Just ListItem.activated
              else Nothing
          )
        & ListItem.setOnClick
          ( EvalModelUpdate $ \st ->
              st
                & #modelData
                . getMoneyOptic boq
                . #modelMoneyCurrencyOpen
                .~ False
                & #modelData
                . getMoneyOptic boq
                . #modelMoneyCurrencySearch
                .~ mempty
                & #modelData
                . getMoneyOptic boq
                . #modelMoneyCurrencyInfo
                .~ item
          )
    )
    [ Miso.text . toMisoString $ inspectCurrencyInfo @Text item
    ]

swapAmountsWidget :: View Action
swapAmountsWidget =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
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
      EvalModelUpdate $ \st ->
        let baseInput =
              st ^. #modelData . #modelDataBaseMoney . #modelMoneyAmountInput
            baseOutput =
              st ^. #modelData . #modelDataBaseMoney . #modelMoneyAmountOutput
            quoteInput =
              st ^. #modelData . #modelDataQuoteMoney . #modelMoneyAmountInput
            quoteOutput =
              st ^. #modelData . #modelDataQuoteMoney . #modelMoneyAmountOutput
         in st
              & #modelData
              . #modelDataBaseMoney
              . #modelMoneyAmountInput
              .~ quoteInput
              & #modelData
              . #modelDataBaseMoney
              . #modelMoneyAmountOutput
              .~ quoteOutput
              & #modelData
              . #modelDataQuoteMoney
              . #modelMoneyAmountInput
              .~ baseInput
              & #modelData
              . #modelDataQuoteMoney
              . #modelMoneyAmountOutput
              .~ baseOutput
              & #modelData
              . #modelDataBaseOrQuote
              .~ Base

swapCurrenciesWidget :: View Action
swapCurrenciesWidget =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
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
      EvalModelUpdate $ \st ->
        let baseCurrency =
              st ^. #modelData . #modelDataBaseMoney . #modelMoneyCurrencyInfo
            quoteCurrency =
              st ^. #modelData . #modelDataQuoteMoney . #modelMoneyCurrencyInfo
         in st
              & #modelData
              . #modelDataBaseMoney
              . #modelMoneyCurrencyInfo
              .~ quoteCurrency
              & #modelData
              . #modelDataQuoteMoney
              . #modelMoneyCurrencyInfo
              .~ baseCurrency
              & #modelData
              . #modelDataBaseOrQuote
              .~ Base

snackbarClosed :: Snackbar.MessageId -> Action
snackbarClosed msg =
  PureModelUpdate (& #modelSnackbarQueue %~ Snackbar.close msg)

inspectMoneyAmount :: (From String a) => Money Rational -> a
inspectMoneyAmount =
  inspectRatio defaultRatioFormat . unMoney

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs
