{-# LANGUAGE CPP #-}

module Main (main) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Ws
#endif
import qualified Data.List.NonEmpty as NonEmpty
import Functora.Money
import Functora.Prelude
import Functora.Rates
import Miso hiding (view)
import qualified Miso
import Miso.String

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp app =
  Warp.runSettings
    ( Warp.setPort
        8080
        (Warp.setTimeout 3600 Warp.defaultSettings)
    )
    =<< JSaddle.jsaddleOr
      Ws.defaultConnectionOptions
      (app >> syncPoint)
      router
  where
    router req sendResp =
      case Wai.pathInfo req of
        ("static" : _) -> staticApp (defaultWebAppSettings ".") req sendResp
        _ -> JSaddle.jsaddleApp req sendResp
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
    modelQuoteCurrencyInfo :: CurrencyInfo
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
            modelQuoteCurrencyInfo = usd
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
          modelQuoteCurrencyInfo = quoteCur
        }

data Action
  = Noop
  | Echo Text
  | SayHelloWorld
  deriving (Show, Eq)

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
          events = defaultEvents,
          initialAction = SayHelloWorld,
          mountPoint = Nothing, -- defaults to 'body'
          logLevel = Off
        }

updateModel :: Action -> Model -> Effect Action Model
-- updateModel AddOne m = noEff (m + 1)
-- updateModel SubtractOne m = noEff (m - 1)
updateModel Noop m = noEff m
updateModel (Echo txt) m =
  m <# do
    liftIO (putStrLn $ "Echo ==> " <> txt) >> pure Noop
updateModel SayHelloWorld m =
  m <# do
    liftIO (putStrLn @Text "Hello World") >> pure Noop

viewModel :: Model -> View Action
viewModel x =
  div_
    []
    [ link_ [rel_ "stylesheet", href_ "static/picnic.min.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      mainWidget x,
      script_ [src_ "static/clipboard.min.js"] mempty,
      script_ [src_ "static/app.js", defer_ "defer"] mempty
    ]

mainWidget :: Model -> View Action
mainWidget st =
  row
    [ fieldset_
        [class_ "flex two"]
        [ input_
            [ type_ "number",
              placeholder_ "Base amount",
              value_
                . toMisoString @Text
                . inspectRatio 8
                . unMoney
                $ modelBaseMoneyAmount st
            ],
          select_ [onInput $ Echo . fromMisoString]
            . toList
            $ modelCurrenciesInfo st
            <&> \cur ->
              option_
                [ textProp "label"
                    . toMisoString @Text
                    $ inspectCurrencyInfo cur
                ]
                [text . ms $ inspect @Text cur]
        ],
      input_
        [ type_ "number",
          placeholder_ "Quote amount",
          value_
            . toMisoString @Text
            . inspectRatio 8
            . unMoney
            $ modelQuoteMoneyAmount st
        ]
    ]

row :: [View action] -> View action
row = div_ [class_ $ "flex one center"]
