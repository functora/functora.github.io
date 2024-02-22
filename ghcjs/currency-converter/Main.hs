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
  { modelFinal :: ModelData,
    modelDraft :: MVar ModelData,
    modelMarket :: MVar Market,
    modelSnackbarQueue :: Snackbar.Queue Action
  }
  deriving stock (Eq, Generic)

data BaseOrQuote
  = Base
  | Quote
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data ModelMoney = ModelMoney
  { modelMoneyAmountInput :: Text,
    modelMoneyAmountOutput :: Money Rational,
    modelMoneyCurrencyInfo :: CurrencyInfo,
    modelMoneyCurrencyOpen :: Bool,
    modelMoneyCurrencySearch :: Text
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

--
-- TODO : bouncy data for amounts only, do not process every amount input!!!
-- But currency and other should not be bouncy, and should be as fast as possible,
-- maybe need to use some loading progress bar for the stuff as curriencies loading
--
data ModelData = ModelData
  { -- TODO : use timestamed data
    modelDataCurrencies :: NonEmpty CurrencyInfo,
    -- TODO : use timestamed data
    modelDataBaseMoney :: ModelMoney,
    modelDataQuoteMoney :: ModelMoney,
    modelDataBaseOrQuote :: BaseOrQuote,
    modelDataUpdatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Data, Generic)

-- significantChange :: ModelData -> ModelData -> Bool
-- significantChange lhs rhs =
--   modelDataCurrencies lhs
--     /= modelDataCurrencies rhs
--     || modelDataBaseAmountOutput lhs
--     /= modelDataBaseAmountOutput rhs
--     || modelDataBaseCurrency lhs
--     /= modelDataBaseCurrency rhs
--     || modelDataQuoteAmountOutput lhs
--     /= modelDataQuoteAmountOutput rhs
--     || modelDataQuoteCurrency lhs
--     /= modelDataQuoteCurrency rhs
--     || modelDataBaseOrQuote lhs
--     /= modelDataBaseOrQuote rhs

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
          { modelDataCurrencies = [btc, usd],
            modelDataBaseMoney =
              ModelMoney
                { modelMoneyAmountInput = inspectMoneyAmount zero,
                  modelMoneyAmountOutput = zero,
                  modelMoneyCurrencyInfo = btc,
                  modelMoneyCurrencyOpen = False,
                  modelMoneyCurrencySearch = mempty
                },
            modelDataQuoteMoney =
              ModelMoney
                { modelMoneyAmountInput = inspectMoneyAmount zero,
                  modelMoneyAmountOutput = zero,
                  modelMoneyCurrencyInfo = usd,
                  modelMoneyCurrencyOpen = False,
                  modelMoneyCurrencySearch = mempty
                },
            modelDataBaseOrQuote = Base,
            modelDataUpdatedAt = ct
          }
  draft <- newMVar final
  let st =
        Model
          { modelFinal = final,
            modelDraft = draft,
            modelMarket = market,
            modelSnackbarQueue = Snackbar.initialQueue
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
            { modelDataCurrencies = currenciesInfo,
              modelDataBaseMoney =
                ModelMoney
                  { modelMoneyAmountInput = inspectMoneyAmount baseAmt,
                    modelMoneyAmountOutput = baseAmt,
                    modelMoneyCurrencyInfo = baseCur,
                    modelMoneyCurrencyOpen = False,
                    modelMoneyCurrencySearch = mempty
                  },
              modelDataQuoteMoney =
                ModelMoney
                  { modelMoneyAmountInput = inspectMoneyAmount quoteAmt,
                    modelMoneyAmountOutput = quoteAmt,
                    modelMoneyCurrencyInfo = quoteCur,
                    modelMoneyCurrencyOpen = False,
                    modelMoneyCurrencySearch = mempty
                  },
              modelDataBaseOrQuote = Base,
              modelDataUpdatedAt = ct
            }
    draft' <- newMVar final'
    pure
      Model
        { modelFinal = final',
          modelDraft = draft',
          modelMarket = market,
          modelSnackbarQueue = Snackbar.initialQueue
        }

data UpdatePolicy
  = InstantUpdate
  | DelayedUpdate
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Data, Generic)

data Action
  = Noop
  | Debounce
  | SetModel Action Model
  | UpdateModelData UpdatePolicy (ModelData -> ModelData)
  | --
    -- TODO : BouncyInput and InstantInput!!! (Or just different inputs for amt cur)
    --
    forall a. (Show a, Data a) => UserInput BaseOrQuote (Lens' ModelData a) a
  | SnackbarClosed Snackbar.MessageId

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
          initialAction = Debounce,
          mountPoint = Nothing, -- defaults to 'body'
          logLevel = Off
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel Noop st = noEff st
updateModel (SetModel action st) _ = st <# pure action
updateModel (SnackbarClosed msg) st =
  noEff $ st & #modelSnackbarQueue %~ Snackbar.close msg
updateModel (UpdateModelData policy updater) st =
  ( if policy == InstantUpdate
      then st & #modelFinal %~ updater
      else st
  )
    <# do
      ct <- getCurrentTime
      modifyMVar (modelDraft st) $ \prev ->
        pure
          ( updater prev & #modelDataUpdatedAt .~ ct,
            Noop
          )
updateModel Debounce st = do
  st <# do
    let ttl = 300 :: Integer
    sleepMilliSeconds ttl
    modifyMVar (modelDraft st) $ \draft -> do
      ct <- getCurrentTime
      let diff = abs . toRational . diffUTCTime ct $ modelDataUpdatedAt draft
      --
      -- TODO : UpdateModelData instead of SetModel ??
      --
      -- if significantChange (modelFinal st) draft && diff > ttl % 1000
      if diff > ttl % 1000
        then do
          -- putStrLn @Text "Debounce"
          output <- liftIO . tryAny . evalModel draft $ modelMarket st
          case output of
            Left e -> do
              let msg =
                    inspect e
                      & Snackbar.message
                      & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
                      & Snackbar.setOnActionIconClick SnackbarClosed
              pure
                ( draft,
                  SetModel Debounce
                    $ st
                    & #modelSnackbarQueue
                    %~ Snackbar.addMessage msg
                )
            Right final ->
              pure
                ( final,
                  SetModel Debounce $ st & #modelFinal .~ final
                )
        else
          pure
            ( draft,
              Debounce
            )
updateModel (UserInput boq optic input) st =
  st <# do
    ct <- getCurrentTime
    modifyMVar (modelDraft st) $ \prev ->
      pure
        ( prev
            & optic
            .~ input
            & #modelDataBaseOrQuote
            .~ boq
            & #modelDataUpdatedAt
            .~ ct,
          Noop
        )

evalModel ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  ModelData ->
  MVar Market ->
  m ModelData
evalModel draft market = do
  let boq = modelDataBaseOrQuote draft
  baseAmtResult <-
    tryAny . parseMoney $ draft ^. getBaseMoneyOptic boq . #modelMoneyAmountInput
  case baseAmtResult of
    Left {} -> pure draft
    Right baseAmt ->
      withMarket market $ do
        let funds =
              Funds
                { fundsMoneyAmount = baseAmt,
                  fundsCurrencyCode =
                    draft
                      ^. getBaseMoneyOptic boq
                      . #modelMoneyCurrencyInfo
                      . #currencyInfoCode
                }
        quote <-
          getQuote funds
            $ draft
            ^. getQuoteMoneyOptic boq
            . #modelMoneyCurrencyInfo
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        pure
          $ draft
          & getBaseMoneyOptic boq
          . #modelMoneyAmountOutput
          .~ baseAmt
          & getQuoteMoneyOptic boq
          . #modelMoneyAmountInput
          .~ inspectMoneyAmount quoteAmt
          & getQuoteMoneyOptic boq
          . #modelMoneyAmountOutput
          .~ quoteAmt

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
    [ LayoutGrid.alignMiddle
    ]
    [ LayoutGrid.inner
        [ class_ "container"
        ]
        [ amountWidget st Base #modelDataBaseMoney,
          currencyWidget st Base,
          amountWidget st Quote #modelDataQuoteMoney,
          currencyWidget st Quote,
          swapAmountsWidget,
          swapCurrenciesWidget,
          Snackbar.snackbar (Snackbar.config SnackbarClosed)
            $ modelSnackbarQueue st
        ]
    ]

amountWidget ::
  Model ->
  BaseOrQuote ->
  Lens' ModelData ModelMoney ->
  View Action
amountWidget st boq optic =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
    ]
    . (: mempty)
    . TextField.outlined
    . TextField.setType (Just "number")
    . TextField.setValid valid
    . TextField.setValue
      ( Just
          . from @Text @String
          $ st
          ^. #modelFinal
          . optic
          . #modelMoneyAmountInput
      )
    . TextField.setOnInput
      ( UserInput boq (optic . #modelMoneyAmountInput) . from @String @Text
      )
    . TextField.setPlaceholder
      ( Just
          . inspectCurrencyInfo
          $ st
          ^. #modelFinal
          . optic
          . #modelMoneyCurrencyInfo
      )
    . TextField.setRequired True
    . TextField.setAttributes [class_ "fill"]
    $ TextField.config
  where
    input = st ^. #modelFinal . optic . #modelMoneyAmountInput
    output = st ^. #modelFinal . optic . #modelMoneyAmountOutput
    valid =
      (parseMoney input == Just output)
        || (input == inspectMoneyAmount output)

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
        ^. #modelFinal
        . getMoneyOptic boq
        . #modelMoneyCurrencyInfo,
      Dialog.dialog
        ( Dialog.config
            & Dialog.setOnClose closed
            & Dialog.setOpen
              ( st
                  ^. #modelFinal
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
                        ^. #modelFinal
                        . getMoneyOptic boq
                        . #modelMoneyCurrencyOpen
                        then id
                        else
                          TextField.setValue
                            ( Just
                                . from @Text @String
                                $ st
                                ^. #modelFinal
                                . getMoneyOptic boq
                                . #modelMoneyCurrencySearch
                            )
                    )
                  . TextField.setOnInput search
                  . TextField.setPlaceholder
                    ( Just
                        . inspectCurrencyInfo
                        $ st
                        ^. #modelFinal
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
      UpdateModelData InstantUpdate $ \st' ->
        st'
          & getMoneyOptic boq
          . #modelMoneyCurrencySearch
          .~ from @String @Text input
    opened =
      UpdateModelData InstantUpdate $ \st' ->
        st'
          & getMoneyOptic boq
          . #modelMoneyCurrencyOpen
          .~ True
          & getMoneyOptic boq
          . #modelMoneyCurrencySearch
          .~ mempty
    closed =
      UpdateModelData InstantUpdate $ \st' ->
        st'
          & getMoneyOptic boq
          . #modelMoneyCurrencyOpen
          .~ False
          & getMoneyOptic boq
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
    currencies = st ^. #modelFinal . #modelDataCurrencies
    current = st ^. #modelFinal . getMoneyOptic boq . #modelMoneyCurrencyInfo
    search = st ^. #modelFinal . getMoneyOptic boq . #modelMoneyCurrencySearch
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
          ( UpdateModelData InstantUpdate $ \st ->
              st
                & getMoneyOptic boq
                . #modelMoneyCurrencyOpen
                .~ False
                & getMoneyOptic boq
                . #modelMoneyCurrencySearch
                .~ mempty
                & getMoneyOptic boq
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
      ( Button.setOnClick event
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
      "Swap amounts"
  where
    event =
      UpdateModelData InstantUpdate $ \st ->
        let baseInput = st ^. #modelDataBaseMoney . #modelMoneyAmountInput
            baseOutput = st ^. #modelDataBaseMoney . #modelMoneyAmountOutput
            quoteInput = st ^. #modelDataQuoteMoney . #modelMoneyAmountInput
            quoteOutput = st ^. #modelDataQuoteMoney . #modelMoneyAmountOutput
         in st
              & #modelDataBaseMoney
              . #modelMoneyAmountInput
              .~ quoteInput
              & #modelDataBaseMoney
              . #modelMoneyAmountOutput
              .~ quoteOutput
              & #modelDataQuoteMoney
              . #modelMoneyAmountInput
              .~ baseInput
              & #modelDataQuoteMoney
              . #modelMoneyAmountOutput
              .~ baseOutput
              & #modelDataBaseOrQuote
              .~ Base

swapCurrenciesWidget :: View Action
swapCurrenciesWidget =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop
    ]
    . (: mempty)
    $ Button.raised
      ( Button.setOnClick event
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
      "Swap currencies"
  where
    event =
      UpdateModelData InstantUpdate $ \st ->
        let baseCurrency = st ^. #modelDataBaseMoney . #modelMoneyCurrencyInfo
            quoteCurrency = st ^. #modelDataQuoteMoney . #modelMoneyCurrencyInfo
         in st
              & #modelDataBaseMoney
              . #modelMoneyCurrencyInfo
              .~ quoteCurrency
              & #modelDataQuoteMoney
              . #modelMoneyCurrencyInfo
              .~ baseCurrency
              & #modelDataBaseOrQuote
              .~ Base

inspectMoneyAmount :: (From String a) => Money Rational -> a
inspectMoneyAmount = inspectRatio defaultRatioFormat . unMoney
