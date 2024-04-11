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
import App.MainWidget
import qualified App.Misc as Misc
import App.Types
import qualified Data.Map as Map
import Functora.Money
import Functora.Prelude as Prelude
import Functora.Rates
import qualified Language.Javascript.JSaddle as JS
import Miso hiding (view)
import qualified Miso
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

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
        Misc.pushActionQueue nextSt $ ChanItem 0 (& #modelHide .~ False)
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
          . Misc.pushActionQueue st
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
        actions <- Misc.drainTChan $ prevSt ^. #modelConsumerQueue
        nextSt <- foldlM (\acc updater -> evalModel $ updater acc) prevSt actions
        pure $ ChanUpdate nextSt
    ]
updateModel (PushUpdate runJSM updater) st = do
  batchEff
    st
    [ do
        Misc.pushActionQueue st updater
        pure Noop,
      do
        runJSM
        pure Noop
    ]

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

syncInputs :: Model -> JSM ()
syncInputs st =
  --
  -- NOTE : The "correct" way is to use "controlled input" with
  -- TextField.setValue but it does cause race conditions
  -- when user types "too fast":
  --
  -- https://github.com/dmjio/miso/issues/272
  --
  forM_ allLens . uncurry $ \uuidLens textLens ->
    JS.eval @Text
      $ "var el = document.getElementById('"
      <> htmlUuid (st ^. cloneLens uuidLens)
      <> "'); if (el && !(el.getElementsByTagName('input')[0] === document.activeElement)) el.value = '"
      <> (st ^. cloneLens textLens)
      <> "';"
  where
    --
    -- TODO : refactor, use TextInput instead of separate fields for money amount
    --
    allLens :: [(ALens' Model UUID, ALens' Model Text)]
    allLens =
      [ ( #modelData . #dataModelTopMoney . #moneyModelAmount . #amountModelUuid,
          #modelData . #dataModelTopMoney . #moneyModelAmount . #amountModelInput
        ),
        ( #modelData . #dataModelBottomMoney . #moneyModelAmount . #amountModelUuid,
          #modelData . #dataModelBottomMoney . #moneyModelAmount . #amountModelInput
        ),
        ( #modelData . #dataModelIssuer . #textModelUuid,
          #modelData . #dataModelIssuer . #textModelData
        ),
        ( #modelData . #dataModelClient . #textModelUuid,
          #modelData . #dataModelClient . #textModelData
        )
      ]

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel st = do
  let loc = st ^. #modelData . #dataModelTopOrBottom
  let baseLens = getBaseConverterMoneyLens loc
  let quoteLens = getQuoteConverterMoneyLens loc
  let baseAmtInput =
        case st ^. cloneLens baseLens . #moneyModelAmount . #amountModelInput of
          amt
            | null amt ->
                inspectRatioDef
                  $ st
                  ^. cloneLens baseLens
                  . #moneyModelAmount
                  . #amountModelOutput
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
                . #moneyModelCurrency
                . #currencyModelData
                . #currencyInfoCode
        quote <-
          getQuote funds
            $ st
            ^. cloneLens quoteLens
            . #moneyModelCurrency
            . #currencyModelData
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        ct <- getCurrentTime
        pure
          $ st
          & cloneLens baseLens
          . #moneyModelAmount
          . #amountModelInput
          .~ baseAmtInput
          & cloneLens baseLens
          . #moneyModelAmount
          . #amountModelOutput
          .~ unTagged baseAmt
          & cloneLens quoteLens
          . #moneyModelAmount
          . #amountModelInput
          .~ inspectRatioDef (unTagged quoteAmt)
          & cloneLens quoteLens
          . #moneyModelAmount
          . #amountModelOutput
          .~ unTagged quoteAmt
          & #modelUpdatedAt
          .~ ct

getBaseConverterMoneyLens :: TopOrBottom -> ALens' Model MoneyModel
getBaseConverterMoneyLens = \case
  Top -> #modelData . #dataModelTopMoney
  Bottom -> #modelData . #dataModelBottomMoney

getQuoteConverterMoneyLens :: TopOrBottom -> ALens' Model MoneyModel
getQuoteConverterMoneyLens = \case
  Top -> #modelData . #dataModelBottomMoney
  Bottom -> #modelData . #dataModelTopMoney

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs
