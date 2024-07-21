{-# LANGUAGE CPP #-}

module Main (main) where

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
import Language.Javascript.JSaddle.Warp as JS
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Ws
import qualified Data.ByteString.Lazy as BL
#endif

#ifdef wasi_HOST_OS
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
#endif

import qualified App.Misc as Misc
import App.Prelude
import App.Types
import App.Widgets.Main
import App.Widgets.Templates
import qualified Data.Generics as Syb
import qualified Data.Map as Map
import Functora.Money hiding (Money)
import Functora.Rates
import qualified Functora.Web as Web
import Language.Javascript.JSaddle ((!), (!!))
import qualified Language.Javascript.JSaddle as JS
import Miso hiding (view)
import qualified Miso
import qualified Text.URI as URI

#ifdef wasi_HOST_OS
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main =
  withUtf8
    . runApp
    . forever
    . handleAny (\e -> maxAttention e >> sleepSeconds 5)
    $ do
      uri <- URI.mkURI . inspect =<< getCurrentURI
      web <- getWebOpts
      st <- newModel web Nothing uri
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

getWebOpts :: JSM Web.Opts
getWebOpts = do
#ifdef wasi_HOST_OS
  ctx <- JS.askJSM
  pure $ Web.defOpts ctx
#else
  pure Web.defOpts
#endif

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
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
        ("web.js" : _) -> staticApp (defaultWebAppSettings "static") req
        ("main.js" : _) -> staticApp (defaultWebAppSettings "static") req
        ("site.webmanifest" : _) -> staticApp (defaultWebAppSettings "static") req
        _ -> JS.jsaddleAppWithJs (JS.jsaddleJs False <> js) req
#endif

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS)
runApp :: IO () -> IO ()
runApp = id
#endif

#ifdef wasi_HOST_OS
runApp :: JSM () -> IO ()
runApp = JSaddle.Wasm.run
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
        Misc.pushActionQueue nextSt $ ChanItem 0 (& #modelLoading .~ False)
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
updateModel SyncInputs st = do
  batchEff
    st
    [ do
        syncInputs st
        pure Noop
    ]
updateModel (ChanUpdate prevSt) _ = do
  batchEff
    prevSt
    [ do
        syncInputs prevSt
        --
        -- NOTE : syncInputs twice, workaround to fix
        -- slow rendering after screen switch.
        --
        sleepMilliSeconds 300
        pure SyncInputs,
      do
        actions <-
          Misc.drainTChan $ prevSt ^. #modelConsumerQueue
        nextSt <-
          handleAny
            ( \e -> do
                maxAttention e
                pure $ prevSt & #modelLoading .~ False
            )
            $ foldlM (\acc updater -> evalModel $ updater acc) prevSt actions
        if nextSt ^. #modelLoading
          then do
            void
              . spawnLink
              . deepseq (viewModel nextSt)
              . Misc.pushActionQueue prevSt
              $ ChanItem 0 (const $ nextSt & #modelLoading .~ False)
            pure
              $ ChanUpdate (prevSt & #modelLoading .~ True)
          else
            pure
              $ ChanUpdate nextSt
    ]
updateModel (PushUpdate newUpdater) st = do
  batchEff
    st
    [ do
        updater <- newUpdater
        Misc.pushActionQueue st updater
        pure Noop
    ]

viewModel :: Model -> View Action
#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
viewModel st =
  div_
    mempty
    [ link_ [rel_ "stylesheet", href_ "static/material-components-web.min.css"],
      link_ [rel_ "stylesheet", href_ "static/material-icons.css"],
      link_ [rel_ "stylesheet", href_ "static/app.css"],
      mainWidget st
    ]
#endif

#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
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
      el <- getElementById . htmlUid @Text $ txt ^. #uniqueUid
      elExist <- ghcjsPure $ JS.isTruthy el
      when elExist $ do
        tf <- el ! ("textField_" :: Text)
        tn <- tf ! ("input_" :: Text) ! ("tagName" :: Text)
        tnIn <- JS.toJSVal ("INPUT" :: Text)
        tnEq <- JS.strictEqual tn tnIn
        unless tnEq $ do
          void $ tf ^. JS.js0 ("destroy" :: Text)
          void $ tf ^. JS.js0 ("initialize" :: Text)
        inps <- el ^. JS.js1 ("getElementsByTagName" :: Text) ("input" :: Text)
        inp <- inps !! 0
        act <- JS.global ! ("document" :: Text) ! ("activeElement" :: Text)
        elActive <- JS.strictEqual inp act
        unless elActive $ el ^. JS.jss ("value" :: Text) (txt ^. #uniqueValue)
      pure txt

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel raw = do
  new <-
    Syb.everywhereM
      ( Syb.mkM $ \cur ->
          withMarket (raw ^. #modelWebOpts) (raw ^. #modelMarket)
            . fmap (fromRight cur)
            . tryMarket
            . getCurrencyInfo (raw ^. #modelWebOpts)
            $ currencyInfoCode cur
      )
      ( raw ^. #modelState
      )
  curs <-
    withMarket (raw ^. #modelWebOpts) (raw ^. #modelMarket)
      . fmap (fromRight $ raw ^. #modelCurrencies)
      . tryMarket
      . fmap (^. #currenciesList)
      $ getCurrencies (raw ^. #modelWebOpts)
  let st = raw & #modelState .~ new & #modelCurrencies .~ curs
  let loc = st ^. #modelState . #stDoc . #stDocConv . #stConvTopOrBottom
  let baseLens = getBaseConverterMoneyLens loc
  let quoteLens = getQuoteConverterMoneyLens loc
  let baseAmtInput =
        case st
          ^. cloneLens baseLens
          . #moneyAmount
          . #fieldInput
          . #uniqueValue of
          amt
            | amt == mempty ->
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
      withMarket (st ^. #modelWebOpts) (st ^. #modelMarket) $ do
        let funds =
              Funds
                baseAmt
                $ st
                ^. cloneLens baseLens
                . #moneyCurrency
                . #currencyOutput
                . #currencyInfoCode
        quote <-
          getQuote (st ^. #modelWebOpts) funds
            $ st
            ^. cloneLens quoteLens
            . #moneyCurrency
            . #currencyOutput
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        ct <- getCurrentTime
        pure
          $ st
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
          & #modelState
          . #stDoc
          . #stDocConv
          . #stConvCreatedAt
          .~ quoteCreatedAt quote
          & #modelOnlineAt
          .~ ct

getBaseConverterMoneyLens :: TopOrBottom -> ALens' Model (Money Unique)
getBaseConverterMoneyLens = \case
  Top -> #modelState . #stDoc . #stDocConv . #stConvTopMoney
  Bottom -> #modelState . #stDoc . #stDocConv . #stConvBottomMoney

getQuoteConverterMoneyLens :: TopOrBottom -> ALens' Model (Money Unique)
getQuoteConverterMoneyLens = \case
  Top -> #modelState . #stDoc . #stDocConv . #stConvBottomMoney
  Bottom -> #modelState . #stDoc . #stDocConv . #stConvTopMoney

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs

maxAttention :: SomeException -> JSM ()
maxAttention e = do
  consoleLog $ inspect e
  alert $ inspect e
