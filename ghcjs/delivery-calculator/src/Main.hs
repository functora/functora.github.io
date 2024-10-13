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
import qualified Data.Map as Map
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
        ("themes" : _) ->
          staticApp (defaultWebAppSettings "../miso-functora/dist") req
        ("fa" : _) ->
          staticApp (defaultWebAppSettings "../miso-functora/lib") req
        ("miso-functora" : _) ->
          staticApp (defaultWebAppSettings "../miso-functora/lib") req
        ("static" : _) ->
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
          $ ChanUpdate (const nextSt)
    ]
updateModel SyncInputs st = do
  batchEff
    st
    [ do
        syncInputs st
        pure Noop
    ]
updateModel (ChanUpdate update0) st0 = do
  let st1 = update0 st0
  batchEff
    st1
    [ do
        --
        -- NOTE : Workaround to fix slow rendering after screen switch.
        --
        sleepMilliSeconds 300
        pure SyncInputs,
      do
        actions <-
          drainTChan $ st1 ^. #modelConsumerQueue
        update1 <-
          foldlM
            ( \acc upd -> do
                fun <- unUpdate upd
                pure $ fun . acc
            )
            id
            actions
        let st2 = update1 st1
        update2 <-
          handleAny
            ( \e -> do
                consoleLog e
                pure $ #modelLoading .~ False
            )
            $ evalModel st2
        let st3 = update2 st2
        uri <- stUri st3
        Jsm.insertStorage ("favorite-" <> vsn) $ st3 ^. #modelFavMap
        Jsm.insertStorage ("current-" <> vsn) uri
        syncUri uri
        nextUri <- stUri $ st3 & #modelState . #stScreen %~ unQrCode
        uriViewer <-
          newFieldPair mempty
            . DynamicFieldText
            . from @Prelude.String @Unicode
            $ URI.renderStr nextUri
        let update3 =
              #modelUriViewer
                %~ mergeFieldPairs
                  [ uriViewer
                      & #fieldPairValue
                      . #fieldOpts
                      . #fieldOptsQrState
                      .~ Just Opened
                  ]
        let st4 = update3 st3
        if st4 ^. #modelLoading
          then do
            void
              . spawnLink
              . deepseq (viewModel st4)
              . pushActionQueue st4
              . Instant
              . PureUpdate
              $ #modelLoading
              .~ False
            pure
              . ChanUpdate
              $ (#modelLoading .~ True)
              . update3
              . update2
              . update1
          else
            pure
              . ChanUpdate
              $ (#modelLoading .~ False)
              . update3
              . update2
              . update1
    ]
updateModel (PushUpdate value) st = do
  case instantOrDelayedValue value of
    PureUpdate f ->
      batchEff
        ( f st
        )
        [ do
            pushActionQueue st . Instant $ PureUpdate id
            pure Noop
        ]
    ImpureUpdate g ->
      batchEff
        st
        [ do
            updater <- g
            pushActionQueue st . Instant $ PureUpdate updater
            pure Noop
        ]
    EffectUpdate e ->
      batchEff
        st
        [ do
            e
            pushActionQueue st . Instant $ PureUpdate id
            pure Noop
        ]
    PureAndImpureUpdate f g ->
      batchEff
        ( f st
        )
        [ do
            updater <- g
            pushActionQueue st . Instant $ PureUpdate updater
            pure Noop
        ]
    PureAndEffectUpdate f e ->
      batchEff
        ( f st
        )
        [ do
            e
            pushActionQueue st . Instant $ PureUpdate id
            pure Noop
        ]

--
-- TODO : !!!
--
-- href_ "https://unpkg.com/nes.css@2.3.0/css/nes.min.css"
-- href_ "node_modules/@lowlighter/matcha/dist/matcha.css"
-- href_ "https://unpkg.com/@sakun/system.css"
-- href_ "https://unpkg.com/terminal.css@0.7.4/dist/terminal.min.css"
-- href_ "https://vinibiavatti1.github.io/TuiCss/dist/tuicss.min.css"
-- href_ "https://fieber.hack.re/fieber.css"
viewModel :: Model -> View Action
#if defined(__GHCJS__) || defined(ghcjs_HOST_OS) || defined(wasi_HOST_OS)
viewModel st =
  prependViews
    ( ( if not (st ^. #modelState . #stEnableTheme)
          then mempty
          else
            [ link_
                [ rel_ "stylesheet",
                  href_ $ "themes/" <> themeCssFile (st ^. #modelState . #stTheme)
                ]
            ]
      ) <>
      [ link_
          [ rel_ "stylesheet",
            href_ $ "miso-functora/miso-functora.min.css"
          ]
      ]
    )
    $ mainWidget st
#else
viewModel st =
  --
  -- NOTE : using non-optimized css for dev purposes only
  --
  prependViews
    (
      [ link_
          [ rel_ "stylesheet",
            href_ $ "miso-functora/pre-theme.css"
          ]
      ] <>
      ( if not (st ^. #modelState . #stEnableTheme)
          then mempty
          else
            [ link_
                [ rel_ "stylesheet",
                  href_ $ "themes/" <> themeCssFile (st ^. #modelState . #stTheme)
                ]
            ]
      ) <>
      [ link_
          [ rel_ "stylesheet",
            href_ $ "miso-functora/post-theme.css"
          ]
      ]
    )
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

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m (Model -> Model)
evalModel prev = do
  let oof = prev ^. #modelState . #stOnlineOrOffline
  let web = prev ^. #modelWebOpts
  case oof of
    Offline -> pure id
    Online ->
      Rates.withMarket web (prev ^. #modelMarket) $ do
        let prevCurs :: [Money.CurrencyInfo] =
              Syb.listify (const True) $ prev ^. #modelState
        --
        -- TODO : don't need this, create map from nextCursList
        --
        nextCursMap :: Map Money.CurrencyCode Money.CurrencyInfo <-
          foldlM
            ( \acc prevCur -> do
                let code =
                      Money.currencyInfoCode prevCur
                nextCur <-
                  fmap (fromRight prevCur)
                    . Rates.tryMarket
                    $ Rates.getCurrencyInfo web code
                pure
                  $ Map.insert code nextCur acc
            )
            mempty
            prevCurs
        nextCursList :: NonEmpty Money.CurrencyInfo <-
          fmap (fromRight $ prev ^. #modelCurrencies)
            . Rates.tryMarket
            . fmap (^. #currenciesList)
            $ Rates.getCurrencies web
        let base =
              Money.Funds
                1
                $ prev
                ^. #modelState
                . #stAssetCurrency
                . #currencyOutput
                . #currencyInfoCode
        let prevQuote =
              Rates.QuoteAt
                { Rates.quoteMoneyAmount =
                    Tagged $ prev ^. #modelState . #stExchangeRate . #fieldOutput,
                  Rates.quoteCreatedAt =
                    prev ^. #modelState . #stExchangeRateAt,
                  Rates.quoteUpdatedAt =
                    prev ^. #modelState . #stExchangeRateAt
                }
        nextQuote <-
          fmap (fromRight prevQuote)
            . Rates.tryMarket
            . Rates.getQuote web base
            $ prev
            ^. #modelState
            . #stMerchantCurrency
            . #currencyOutput
            . #currencyInfoCode
        let rateValue =
              unTagged $ nextQuote ^. #quoteMoneyAmount
        let rateUpdated =
              nextQuote ^. #quoteCreatedAt
        pure
          $ ( #modelState
                %~ Syb.everywhere
                  ( Syb.mkT $ \cur ->
                      fromMaybe cur
                        $ Map.lookup (cur ^. #currencyInfoCode) nextCursMap
                  )
            )
          . ( #modelCurrencies
                .~ nextCursList
            )
          . ( #modelState
                . #stExchangeRate
                . #fieldInput
                . #uniqueValue
                .~ inspectRatioDef rateValue
            )
          . ( #modelState
                . #stExchangeRate
                . #fieldOutput
                .~ rateValue
            )
          . ( #modelState
                . #stExchangeRateAt
                .~ rateUpdated
            )

-- new <-
--   case oof of
--     Online ->
--       Syb.everywhereM
--         ( Syb.mkM $ \cur ->
--             Rates.withMarket (prev ^. #modelWebOpts) (prev ^. #modelMarket)
--               . fmap (fromRight cur)
--               . Rates.tryMarket
--               . Rates.getCurrencyInfo (prev ^. #modelWebOpts)
--               $ Money.currencyInfoCode cur
--         )
--         ( prev ^. #modelState
--         )
--     Offline ->
--       pure $ prev ^. #modelState
-- curs <-
--   case oof of
--     Online ->
--       Rates.withMarket (prev ^. #modelWebOpts) (prev ^. #modelMarket)
--         . fmap (fromRight $ prev ^. #modelCurrencies)
--         . Rates.tryMarket
--         . fmap (^. #currenciesList)
--         $ Rates.getCurrencies (prev ^. #modelWebOpts)
--     Offline ->
--       pure $ prev ^. #modelCurrencies
-- let next =
--       prev
--         & #modelState
--         .~ new
--         & #modelCurrencies
--         .~ curs
-- case oof of
--   Offline -> pure next
--   Online ->
--     Rates.withMarket (next ^. #modelWebOpts) (next ^. #modelMarket) $ do
--       let base =
--             Money.Funds
--               1
--               $ next
--               ^. #modelState
--               . #stAssetCurrency
--               . #currencyOutput
--               . #currencyInfoCode
--       quote <-
--         Rates.getQuote (next ^. #modelWebOpts) base
--           $ next
--           ^. #modelState
--           . #stMerchantCurrency
--           . #currencyOutput
--           . #currencyInfoCode
--       let rate =
--             unTagged
--               $ quote
--               ^. #quoteMoneyAmount
--       pure
--         $ next
--         & #modelState
--         . #stExchangeRate
--         . #fieldInput
--         . #uniqueValue
--         .~ inspectRatioDef rate
--         & #modelState
--         . #stExchangeRate
--         . #fieldOutput
--         .~ rate
--         & #modelState
--         . #stExchangeRateAt
--         .~ (quote ^. #quoteCreatedAt)

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
