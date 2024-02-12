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
import Functora.Prelude
import Functora.Rates
import qualified Material.Button as Button
import qualified Material.Card as Card
import qualified Material.Elevation as Evaluation
import qualified Material.IconButton as IconButton
import qualified Material.Select as Select
import qualified Material.Select.Item as SelectItem
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import qualified Miso
import Miso.String

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
  { modelMarket :: MVar Market,
    -- TODO : use timestamed data
    modelCurrenciesInfo :: NonEmpty CurrencyInfo,
    modelBaseMoneyAmount :: Money Rational,
    modelBaseCurrencyInfo :: CurrencyInfo,
    -- TODO : use timestamed data
    modelQuoteMoneyAmount :: Money Rational,
    modelQuoteCurrencyInfo :: CurrencyInfo,
    modelDebug :: Text
  }
  deriving stock (Eq, Data, Generic)

mkModel :: (MonadThrow m, MonadUnliftIO m) => m Model
mkModel = do
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
  let st =
        Model
          { modelMarket = market,
            modelCurrenciesInfo = [btc, usd],
            modelBaseMoneyAmount = Money 0,
            modelBaseCurrencyInfo = btc,
            modelQuoteMoneyAmount = Money 0,
            modelQuoteCurrencyInfo = usd,
            modelDebug = mempty
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
    pure
      Model
        { modelMarket = market,
          modelCurrenciesInfo = currenciesInfo,
          modelBaseMoneyAmount = baseAmt,
          modelBaseCurrencyInfo = baseCur,
          modelQuoteMoneyAmount = quoteMoneyAmount quote,
          modelQuoteCurrencyInfo = quoteCur,
          modelDebug = mempty
        }

data Action
  = Noop
  | Debug Text
  | SayHelloWorld
  deriving (Show, Eq)

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
          initialAction = SayHelloWorld,
          mountPoint = Nothing, -- defaults to 'body'
          logLevel = Off
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel Noop m = noEff m
updateModel (Debug txt) m =
  m {modelDebug = txt} <# do
    pure Noop
updateModel SayHelloWorld m =
  m <# do
    liftIO (putStrLn @Text "Hello World") >> pure Noop

viewModel :: Model -> View Action
viewModel x =
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
      mainWidget x,
      script_ [src_ "static/clipboard.min.js"] mempty,
      script_
        [ src_
            "https://unpkg.com/material-components-web@6.0.0/dist/material-components-web.min.js"
        ]
        mempty,
      script_ [src_ "static/app.js", defer_ "defer"] mempty
    ]

currencyInfoItem :: CurrencyInfo -> SelectItem.SelectItem CurrencyInfo Action
currencyInfoItem cur =
  SelectItem.selectItem
    (SelectItem.config $ cur)
    [Miso.text . toMisoString $ inspectCurrencyInfo @Text cur]

mainWidget :: Model -> View Action
mainWidget st =
  div_
    mempty
    [ TextField.outlined
        . TextField.setLabel (Just "Hi")
        . TextField.setOnInput (Debug . inspect @Text)
        $ TextField.config,
      Select.outlined
        ( Select.setLabel (Just "Choose wisely")
            $ Select.setOnChange (Debug . inspect @Text)
            $ Select.setSelected (Just $ modelBaseCurrencyInfo st) Select.config
        )
        (currencyInfoItem . NonEmpty.head $ modelCurrenciesInfo st)
        . fmap currencyInfoItem
        . NonEmpty.tail
        $ modelCurrenciesInfo st,
      Card.card
        ( Card.setAttributes
            [ style_ $ Map.singleton "margin" "48px 0",
              style_ $ Map.singleton "width" "350px",
              Evaluation.z10
            ]
            $ Card.config
        )
        Card.Content
          { Card.blocks =
              [ Card.Block
                  $ div_
                    [style_ $ Map.singleton "padding" "1rem"]
                    [ h2_
                        [ Typography.headline6,
                          style_ $ Map.singleton "margin" "0"
                        ]
                        [Miso.text "Title"],
                      h3_
                        [ Typography.subtitle2,
                          Theme.textSecondaryOnBackground,
                          style_ $ Map.singleton "margin" "0"
                        ]
                        [Miso.text "Subtitle"]
                    ],
                Card.Block
                  $ div_
                    []
                    [ p_
                        [ Typography.body2,
                          Theme.textSecondaryOnBackground,
                          style_ $ Map.singleton "padding" "0 1rem 0.5rem 1rem",
                          style_ $ Map.singleton "margin" "0"
                        ]
                        [Miso.text . toMisoString $ modelDebug st]
                    ]
              ],
            Card.actions =
              Just
                $ Card.cardActions
                  [Card.button Button.config "Visit"]
                  [Card.icon IconButton.config "favorite"]
          }
    ]

-- row
--   [ fieldset_
--       [class_ "flex two"]
--       [ input_
--           [ type_ "number",
--             placeholder_ "Base amount",
--             value_
--               . toMisoString @Text
--               . inspectRatio 8
--               . unMoney
--               $ modelBaseMoneyAmount st
--           ],
--         -- datalist_ [id_ "base"]
--         --   . toList
--         --   $ modelCurrenciesInfo st
--         --   <&> \cur ->
--         --     option_
--         --       [ textProp "label"
--         --           . toMisoString @Text
--         --           $ inspectCurrencyInfo cur
--         --       ]
--         --       [text . ms $ inspect @Text cur],
--         -- input_ [list_ "base"]
--         select_
--           [ onInput $ Echo . fromMisoString
--           ]
--           . toList
--           $ modelCurrenciesInfo st
--           <&> \cur ->
--             option_
--               [ textProp "label"
--                   . toMisoString @Text
--                   $ inspectCurrencyInfo cur
--               ]
--               [text . ms $ inspect @Text cur]
--       ],
--     input_
--       [ type_ "number",
--         placeholder_ "Quote amount",
--         value_
--           . toMisoString @Text
--           . inspectRatio 8
--           . unMoney
--           $ modelQuoteMoneyAmount st
--       ]
--   ]

-- row :: [View action] -> View action
-- row = div_ [class_ $ "flex one center"]
