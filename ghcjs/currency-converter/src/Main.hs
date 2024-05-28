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
import qualified App.Misc as Misc
import App.Types
import App.Widgets.Main
import qualified Data.Generics as Syb
import qualified Data.Map as Map
import Functora.Money hiding (Money)
import Functora.Prelude hiding (Field)
import Functora.Rates
import qualified Language.Javascript.JSaddle as JS
import Miso hiding (view)
import qualified Miso
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Text.URI as URI

main :: IO ()
main =
  runApp . forever . handleAny (\e -> maxAttention e >> sleepSeconds 5) $ do
    uri <- URI.mkURI . inspect =<< getCurrentURI
    st <- newModel uri
    startApp
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
        unless (upToDate ct $ st ^. #modelOnlineAt)
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
        actions <-
          Misc.drainTChan $ prevSt ^. #modelConsumerQueue
        nextSt <-
          handleAny
            ( \e -> do
                maxAttention e
                pure $ prevSt & #modelHide .~ False
            )
            $ foldlM (\acc updater -> evalModel $ updater acc) prevSt actions
        pure
          $ ChanUpdate nextSt
    ]
updateModel (PushUpdate newUpdater) st = do
  batchEff
    st
    [ do
        updater <- newUpdater
        Misc.pushActionQueue st updater
        void . spawnLink $ do
          sleepMilliSeconds 300
          Misc.pushActionQueue st $ ChanItem 0 id
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

--
-- NOTE : The "correct" way is to use "controlled input" with
-- TextField.setValue but it does cause race conditions
-- when user types "too fast":
--
-- https://github.com/dmjio/miso/issues/272
--
syncInputs :: Model -> JSM ()
syncInputs =
  void
    . Syb.everywhereM (Syb.mkM fun)
    . modelState
  where
    fun :: Unique Text -> JSM (Unique Text)
    fun txt = do
      void
        . JS.eval @Text
        $ "var el = document.getElementById('"
        <> htmlUid (txt ^. #uniqueUid)
        <> "'); if (el && !(el.getElementsByTagName('input')[0] === document.activeElement)) el.value = '"
        <> (txt ^. #uniqueValue)
        <> "';"
      pure txt

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel raw = do
  new <-
    Syb.everywhereM
      ( Syb.mkM $ \cur ->
          withMarket (raw ^. #modelMarket)
            . fmap (fromRight cur)
            . tryMarket
            . getCurrencyInfo
            $ currencyInfoCode cur
      )
      ( raw ^. #modelState
      )
  let st = raw & #modelState .~ new
  let loc = st ^. #modelState . #stConv . #stConvTopOrBottom
  let baseLens = getBaseConverterMoneyLens loc
  let quoteLens = getQuoteConverterMoneyLens loc
  let baseAmtInput =
        case st
          ^. cloneLens baseLens
          . #moneyAmount
          . #fieldInput
          . #uniqueValue of
          amt
            | null amt ->
                inspectRatioDef
                  $ st
                  ^. cloneLens baseLens
                  . #moneyAmount
                  . #fieldOutput
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
                . #moneyCurrency
                . #currencyOutput
                . #currencyInfoCode
        quote <-
          getQuote funds
            $ st
            ^. cloneLens quoteLens
            . #moneyCurrency
            . #currencyOutput
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        next <- evalInvoice st
        ct <- getCurrentTime
        pure
          $ next
          & cloneLens baseLens
          . #moneyAmount
          . #fieldInput
          . #uniqueValue
          .~ baseAmtInput
          & cloneLens baseLens
          . #moneyAmount
          . #fieldOutput
          .~ unTagged baseAmt
          & cloneLens quoteLens
          . #moneyAmount
          . #fieldInput
          . #uniqueValue
          .~ inspectRatioDef (unTagged quoteAmt)
          & cloneLens quoteLens
          . #moneyAmount
          . #fieldOutput
          .~ unTagged quoteAmt
          & #modelOnlineAt
          .~ ct

evalInvoice ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Model ->
  ReaderT (MVar Market) m Model
evalInvoice st = do
  mtds <- forM (st ^. #modelState . #stDoc . #stDocPaymentMethods) $ \mtd -> do
    total <-
      foldM
        (\acc -> fmap (+ acc) . getTotalPayment mtd)
        0
        (st ^. #modelState . #stDoc . #stDocAssets)
    pure
      $ mtd
      & #paymentMethodMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef @Text total
      & #paymentMethodMoney
      . #moneyAmount
      . #fieldOutput
      .~ total
  pure
    $ st
    & #modelState
    . #stDoc
    . #stDocPaymentMethods
    .~ mtds

getTotalPayment ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  PaymentMethod Unique ->
  Asset Unique ->
  ReaderT (MVar Market) m Rational
getTotalPayment method asset = do
  quote <-
    getQuote
      ( Funds (Tagged total)
          $ asset
          ^. #assetPrice
          . #moneyCurrency
          . #currencyOutput
          . #currencyInfoCode
      )
      ( method
          ^. #paymentMethodMoney
          . #moneyCurrency
          . #currencyOutput
          . #currencyInfoCode
      )
  pure $ quote ^. #quoteMoneyAmount . to unTagged
  where
    price = asset ^. #assetPrice . #moneyAmount . #fieldOutput
    total =
      foldl
        ( \acc Field {fieldType = typ, fieldOutput = out} ->
            case out of
              DynamicFieldNumber x
                | typ == FieldTypeNumber ->
                    acc * x
              DynamicFieldNumber x
                | typ == FieldTypePercent ->
                    acc * (1 + (x / 100))
              _ ->
                acc
        )
        price
        ( fmap (^. #fieldPairValue) $ asset ^. #assetFieldPairs
        )

getBaseConverterMoneyLens :: TopOrBottom -> ALens' Model (Money Unique)
getBaseConverterMoneyLens = \case
  Top -> #modelState . #stConv . #stConvTopMoney
  Bottom -> #modelState . #stConv . #stConvBottomMoney

getQuoteConverterMoneyLens :: TopOrBottom -> ALens' Model (Money Unique)
getQuoteConverterMoneyLens = \case
  Top -> #modelState . #stConv . #stConvBottomMoney
  Bottom -> #modelState . #stConv . #stConvTopMoney

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs

maxAttention :: SomeException -> JSM ()
maxAttention e = do
  consoleLog $ inspect e
  alert $ inspect e
