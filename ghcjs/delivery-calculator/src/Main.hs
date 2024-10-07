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

import App.Types
import App.Widgets.Main
import App.Widgets.Templates
import qualified Data.Generics as Syb
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Money as Money
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
      uri <- URI.mkURI . inspect =<< getCurrentURI
      mSt <- unShareUri uri
      web <- getWebOpts
      st <- newModel web Nothing uri
      startApp
        App
          { model = st,
            update = updateModel,
            Miso.view = viewModel,
            subs = mempty,
            events = defaultEvents,
            initialAction = InitUpdate mSt,
            mountPoint = Nothing,
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
  js0 <- BL.readFile "../miso-functora/js/main.min.js"
  js1 <- BL.readFile "static/app.js"
  Warp.runSettings
    ( Warp.setPort
        8080
        (Warp.setTimeout 3600 Warp.defaultSettings)
    )
    =<< JS.jsaddleOr
      Ws.defaultConnectionOptions
      (app >> syncPoint)
      (router $ js0 <> js1)
  where
    router js req =
      case Wai.pathInfo req of
        ("static" : _) ->
          staticApp (defaultWebAppSettings ".") req
        ("node_modules" : _) ->
          staticApp (defaultWebAppSettings ".") req
        ("favicon.ico" : _) ->
          staticApp (defaultWebAppSettings "static") req
        ("site.webmanifest" : _) ->
          staticApp (defaultWebAppSettings "static") req
        (file : _) | (isSuffixOf ".js" file) && (file /= "jsaddle.js") ->
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
              pushActionQueue
                nextSt
                . Instant
                . PureUpdate
                $ (& #modelFavMap %~ fav)
                . (& #modelLoading .~ False)
            else Jsm.selectStorage ("current-" <> vsn) $ \case
              Nothing ->
                pushActionQueue nextSt
                  . Instant
                  . PureUpdate
                  $ (& #modelFavMap %~ fav)
                  . (& #modelLoading .~ False)
              Just uri -> do
                finSt <- newModel (nextSt ^. #modelWebOpts) (Just nextSt) uri
                pushActionQueue nextSt
                  . Instant
                  . PureUpdate
                  $ ( const
                        $ finSt
                        & #modelFavMap
                        %~ fav
                        & #modelLoading
                        .~ False
                    )
        pure
          $ ChanUpdate nextSt
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
            =<< foldlM evalUpdate prevSt actions
        uri <- stUri nextSt
        Jsm.insertStorage ("favorite-" <> vsn) (nextSt ^. #modelFavMap)
        Jsm.insertStorage ("current-" <> vsn) uri
        syncUri uri
        nextUri <- stUri $ nextSt & #modelState . #stScreen %~ unQrCode
        uriViewer <-
          newFieldPair mempty
            . DynamicFieldText
            . from @Prelude.String @Unicode
            $ URI.renderStr nextUri
        let finSt =
              nextSt
                & #modelUriViewer
                %~ mergeFieldPairs
                  [ uriViewer
                      & #fieldPairValue
                      . #fieldOpts
                      . #fieldOptsQrState
                      .~ Just Opened
                  ]
        if finSt ^. #modelLoading
          then do
            void
              . spawnLink
              . deepseq (viewModel finSt)
              . pushActionQueue prevSt
              . Instant
              . PureUpdate
              . const
              $ finSt
              & #modelLoading
              .~ False
            pure
              $ ChanUpdate (prevSt & #modelLoading .~ True)
          else
            pure
              $ ChanUpdate finSt
    ]
updateModel (PushUpdate updater) st = do
  batchEff
    st
    [ do
        pushActionQueue st updater
        pure Noop
    ]

viewModel :: Model -> View Action
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
viewModel st =
  mainWidget st
#else
viewModel st =
  prependViews
    [ link_
        [ rel_ "stylesheet",
          href_ "static/css/fontawesome.min.css"
        ],
      link_
        [ rel_ "stylesheet",
          -- href_ "static/css/mvp.css"
          -- href_ "static/css/simple.min.css"
          href_ "static/css/tacit-css-1.8.1.min.css"
          -- href_ "node_modules/@lowlighter/matcha/dist/matcha.css"
        ],
      link_
        [ rel_ "stylesheet",
          href_ "static/css/app.css"
        ]
    ]
    $ mainWidget st
#endif

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
    . JS.eval @Unicode
    $ "Array.from(document.getElementsByTagName('mdc-text-field')).forEach( function (x) { if ( (x.getElementsByTagName('input')[0] && x.textField_.input_.tagName != 'INPUT') || (x.getElementsByTagName('textarea')[0] && x.textField_.input_.tagName != 'TEXTAREA')) { x.textField_.destroy(); x.textField_.initialize(); } });"
  void
    . Syb.everywhereM (Syb.mkM fun)
    $ modelState st
  where
    fun :: Unique Unicode -> JSM (Unique Unicode)
    fun txt = do
      el <-
        getElementById
          . either impureThrow id
          . decodeUtf8Strict
          . unTagged
          . htmlUid
          $ txt
          ^. #uniqueUid
      elExist <- ghcjsPure $ JS.isTruthy el
      when elExist $ do
        inps <-
          el
            ^. JS.js1
              ("getElementsByTagName" :: Unicode)
              ("input" :: Unicode)
        inp <- inps !! 0
        act <-
          JS.global
            ! ("document" :: Unicode)
            ! ("activeElement" :: Unicode)
        elActive <- JS.strictEqual inp act
        unless elActive $ el ^. JS.jss ("value" :: Unicode) (txt ^. #uniqueValue)
      pure txt

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel prev = do
  let oof = prev ^. #modelState . #stOnlineOrOffline
  new <-
    case oof of
      Online ->
        Syb.everywhereM
          ( Syb.mkM $ \cur ->
              Rates.withMarket (prev ^. #modelWebOpts) (prev ^. #modelMarket)
                . fmap (fromRight cur)
                . Rates.tryMarket
                . Rates.getCurrencyInfo (prev ^. #modelWebOpts)
                $ Money.currencyInfoCode cur
          )
          ( prev ^. #modelState
          )
      Offline ->
        pure $ prev ^. #modelState
  curs <-
    case oof of
      Online ->
        Rates.withMarket (prev ^. #modelWebOpts) (prev ^. #modelMarket)
          . fmap (fromRight $ prev ^. #modelCurrencies)
          . Rates.tryMarket
          . fmap (^. #currenciesList)
          $ Rates.getCurrencies (prev ^. #modelWebOpts)
      Offline ->
        pure $ prev ^. #modelCurrencies
  let next =
        prev
          & #modelState
          .~ new
          & #modelCurrencies
          .~ curs
  case oof of
    Offline -> pure next
    Online ->
      Rates.withMarket (next ^. #modelWebOpts) (next ^. #modelMarket) $ do
        let base =
              Money.Funds
                1
                $ next
                ^. #modelState
                . #stAssetCurrency
                . #currencyOutput
                . #currencyInfoCode
        quote <-
          Rates.getQuote (next ^. #modelWebOpts) base
            $ next
            ^. #modelState
            . #stMerchantCurrency
            . #currencyOutput
            . #currencyInfoCode
        let rate =
              unTagged
                $ quote
                ^. #quoteMoneyAmount
        pure
          $ next
          & #modelState
          . #stExchangeRate
          . #fieldInput
          . #uniqueValue
          .~ inspectRatioDef rate
          & #modelState
          . #stExchangeRate
          . #fieldOutput
          .~ rate
          & #modelState
          . #stExchangeRateAt
          .~ (quote ^. #quoteCreatedAt)

syncUri :: URI -> JSM ()
syncUri uri = do
  textUri <- fmap inspect getCurrentURI
  prevUri <- URI.mkURI textUri
  let nextUri = prevUri {URI.uriQuery = URI.uriQuery uri}
  when (nextUri /= prevUri)
    $ pushURI
    =<< ( maybe (throwString $ "Bad URI " <> textUri) pure
            . URI.parseURI
            $ URI.renderStr nextUri
        )
