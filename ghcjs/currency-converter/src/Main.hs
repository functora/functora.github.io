{-# LANGUAGE CPP #-}

module Main (main) where

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
import Language.Javascript.JSaddle.Warp as JS
import qualified Network.Wai as Wai
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Ws
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
#endif

#ifdef wasi_HOST_OS
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
#endif

import qualified App.Misc as Misc
import App.Types
import App.Widgets.Main
import App.Widgets.Templates
import qualified Data.Generics as Syb
import qualified Data.Map as Map
import qualified Functora.Aes as Aes
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import Functora.Money hiding (Money)
import qualified Functora.Prelude as Prelude
import qualified Functora.Rates as Rates
import qualified Functora.Web as Web
import Language.Javascript.JSaddle ((!), (!!))
import qualified Language.Javascript.JSaddle as JS
import qualified Miso
import qualified Network.URI as URI (parseURI)
import qualified Text.URI as URI

#ifdef wasi_HOST_OS
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main =
  withUtf8
    . runApp
    . forever
    . handleAny (\e -> consoleLog e >> sleepSeconds 5)
    $ do
      uri <- URI.mkURI . Prelude.inspect =<< getCurrentURI
      mSt <- unShareUri uri
      web <- getWebOpts
      st <- newModel web Nothing uri
      startApp
        App
          { model = st,
            update = updateModel,
            Miso.view = viewModel,
            subs = mempty,
            events = extendedEvents,
            initialAction = InitUpdate $ mSt ^? _Just . #stCpt . _Just,
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
  let cap = "const cap = document.createElement('script'); cap.language = 'javascript'; cap.src = 'main.js'; cap.defer = 'defer'; cap.type = 'module'; document.getElementsByTagName('head')[0].appendChild(cap);" :: BL.ByteString
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
      (router $ cap <> js0 <> js1 <> js2 )
  where
    router js req =
      case Wai.pathInfo req of
        ("static" : _) ->
          staticApp (defaultWebAppSettings ".") req
        ("favicon.ico" : _) ->
          staticApp (defaultWebAppSettings "static") req
        ("site.webmanifest" : _) ->
          staticApp (defaultWebAppSettings "static") req
        (file : _) | (T.isSuffixOf ".js" file) && (file /= "jsaddle.js") ->
          staticApp (defaultWebAppSettings "static") req
        _ ->
          JS.jsaddleAppWithJs (JS.jsaddleJs False <> js) req
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
updateModel (InitUpdate ext) prevSt = do
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
        Jsm.selectStorage ("favorite-" <> vsn) $ \mFav -> do
          let fav = mergeMap (const id) $ fromMaybe mempty mFav
          if isJust ext
            then
              Misc.pushActionQueue
                nextSt
                . Instant
                $ pure
                . (& #modelFavMap %~ fav)
                . (& #modelLoading .~ False)
            else Jsm.selectStorage ("current-" <> vsn) $ \case
              Nothing ->
                Misc.pushActionQueue nextSt
                  . Instant
                  $ pure
                  . (& #modelFavMap %~ fav)
                  . (& #modelLoading .~ False)
              Just uri -> do
                finSt <- newModel (nextSt ^. #modelWebOpts) (Just nextSt) uri
                Misc.pushActionQueue nextSt
                  $ Instant
                    ( const
                        . pure
                        $ finSt
                        & #modelFavMap
                        %~ fav
                        & #modelLoading
                        .~ False
                    )
        pure
          $ ChanUpdate nextSt
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
          . Instant
          $ pure
          . id
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
        --
        -- NOTE : Workaround to fix slow rendering after screen switch.
        --
        sleepMilliSeconds 300
        pure SyncInputs,
      do
        actions <-
          drainTChan $ prevSt ^. #modelConsumerQueue
        nextSt <-
          handleAny
            ( \e -> do
                consoleLog e
                pure $ prevSt & #modelLoading .~ False
            )
            $ evalModel
            =<< foldlM (&) prevSt actions
        uri <- URI.mkURI $ shareLink nextSt
        Jsm.insertStorage ("favorite-" <> vsn) (nextSt ^. #modelFavMap)
        Jsm.insertStorage ("current-" <> vsn) uri
        syncUri uri
        if nextSt ^. #modelLoading
          then do
            void
              . spawnLink
              . deepseq (viewModel nextSt)
              . Misc.pushActionQueue prevSt
              $ Instant (const . pure $ nextSt & #modelLoading .~ False)
            pure
              $ ChanUpdate (prevSt & #modelLoading .~ True)
          else
            pure
              $ ChanUpdate nextSt
    ]
updateModel (PushUpdate updater) st = do
  batchEff
    st
    [ do
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
syncInputs st = do
  void
    . JS.eval @MisoString
    $ "Array.from(document.getElementsByTagName('mdc-text-field')).forEach( function (x) { if ( (x.getElementsByTagName('input')[0] && x.textField_.input_.tagName != 'INPUT') || (x.getElementsByTagName('textarea')[0] && x.textField_.input_.tagName != 'TEXTAREA')) { x.textField_.destroy(); x.textField_.initialize(); } });"
  void
    . Syb.everywhereM (Syb.mkM fun)
    $ modelState st
  void
    . Syb.everywhereM (Syb.mkM fun)
    $ modelFavName st
  where
    fun :: Unique MisoString -> JSM (Unique MisoString)
    fun txt = do
      el <- getElementById . htmlUid @MisoString $ txt ^. #uniqueUid
      elExist <- ghcjsPure $ JS.isTruthy el
      when elExist $ do
        inps <-
          el ^. JS.js1 ("getElementsByTagName" :: MisoString) ("input" :: MisoString)
        inp <- inps !! 0
        act <- JS.global ! ("document" :: MisoString) ! ("activeElement" :: MisoString)
        elActive <- JS.strictEqual inp act
        unless elActive $ el ^. JS.jss ("value" :: MisoString) (txt ^. #uniqueValue)
      pure txt

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel raw = do
  let oof = raw ^. #modelState . #stDoc . #stDocOnlineOrOffline
  new <-
    case oof of
      Online ->
        Syb.everywhereM
          ( Syb.mkM $ \cur ->
              Rates.withMarket (raw ^. #modelWebOpts) (raw ^. #modelMarket)
                . fmap (fromRight cur)
                . Rates.tryMarket
                . Rates.getCurrencyInfo (raw ^. #modelWebOpts)
                $ currencyInfoCode cur
          )
          ( raw ^. #modelState
          )
      Offline ->
        pure $ raw ^. #modelState
  curs <-
    case oof of
      Online ->
        Rates.withMarket (raw ^. #modelWebOpts) (raw ^. #modelMarket)
          . fmap (fromRight $ raw ^. #modelCurrencies)
          . Rates.tryMarket
          . fmap (^. #currenciesList)
          $ Rates.getCurrencies (raw ^. #modelWebOpts)
      Offline ->
        pure $ raw ^. #modelCurrencies
  km <-
    if (new ^. #stKm . #kmIkm . #unIkm == mempty)
      && (new ^. #stIkm . #fieldOutput == mempty)
      && isNothing (new ^. #stCpt)
      then Aes.randomKm 32
      else pure $ new ^. #stKm
  let st =
        raw
          & #modelState
          .~ (new & #stKm .~ km)
          & #modelCurrencies
          .~ curs
  let loc = st ^. #modelState . #stDoc . #stDocTopOrBottom
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
      case oof of
        Offline ->
          pure st
        Online ->
          Rates.withMarket (st ^. #modelWebOpts) (st ^. #modelMarket) $ do
            let funds =
                  Funds
                    baseAmt
                    $ st
                    ^. cloneLens baseLens
                    . #moneyCurrency
                    . #currencyOutput
                    . #currencyInfoCode
            quote <-
              Rates.getQuote (st ^. #modelWebOpts) funds
                $ st
                ^. cloneLens quoteLens
                . #moneyCurrency
                . #currencyOutput
                . #currencyInfoCode
            let quoteAmt = quote ^. #quoteMoneyAmount
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
              . #stDocCreatedAt
              .~ (quote ^. #quoteCreatedAt)
              & #modelOnlineAt
              .~ ct

syncUri :: URI -> JSM ()
syncUri uri = do
  textUri <- fmap Prelude.inspect getCurrentURI
  prevUri <- URI.mkURI textUri
  let nextUri = prevUri {URI.uriQuery = URI.uriQuery uri}
  when (nextUri /= prevUri)
    $ pushURI
    =<< ( maybe (throwString $ "Bad URI " <> textUri) pure
            . URI.parseURI
            . from @Prelude.Text @Prelude.String
            $ URI.render nextUri
        )

getBaseConverterMoneyLens :: TopOrBottom -> ALens' Model (Money Unique)
getBaseConverterMoneyLens = \case
  Top -> #modelState . #stDoc . #stDocTopMoney
  Bottom -> #modelState . #stDoc . #stDocBottomMoney

getQuoteConverterMoneyLens :: TopOrBottom -> ALens' Model (Money Unique)
getQuoteConverterMoneyLens = \case
  Top -> #modelState . #stDoc . #stDocBottomMoney
  Bottom -> #modelState . #stDoc . #stDocTopMoney

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs
