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

import App.Init
import App.Types
import App.Widgets.Main
import qualified Data.Generics as Syb
import qualified Data.Map as Map
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import Language.Javascript.JSaddle ((!))
import qualified Language.Javascript.JSaddle as JS
import qualified Miso

#ifdef wasi_HOST_OS
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main =
  withUtf8
    . runApp
    . forever
    . handleAny
      ( \e -> do
          alert . from @String @Unicode $ displayException e
          sleepSeconds 5
      )
    $ do
      sink <- newEmptyMVar
      st <- newModel sink Nothing Nothing
      startApp
        App
          { model = st,
            update = updateModel,
            Miso.view = viewModel,
            subs = mempty,
            events = Map.insert "focus" True defaultEvents,
            initialAction = InitUpdate,
            mountPoint = Nothing,
            logLevel = Off
          }

#if !defined(__GHCJS__) && !defined(ghcjs_HOST_OS) && !defined(wasi_HOST_OS)
runApp :: JSM () -> IO ()
runApp app = do
  js0 <- BL.readFile "../miso-functora/js/main.min.js"
  js1 <- BL.readFile "static/app.js"
  js2 <- BL.readFile "static/telegram-web-app-58.js"
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
        (file : _) | isSuffixOf ".js" file && (file /= "jsaddle.js") ->
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
updateModel InitUpdate prevSt = do
  effectSub prevSt $ \sink -> do
    mvSink <- newMVar sink
    let nextSt = prevSt {modelSink = mvSink}
    finSt <- newModel mvSink (Just nextSt) . Just $ modelState nextSt
    liftIO
      . sink
      . PushUpdate
      . PureUpdate
      . const
      $ finSt
      & #modelLoading
      .~ False
    liftIO
      . sink
      . PushUpdate
      $ PureUpdate (const nextSt)
updateModel SyncInputs st = do
  batchEff
    st
    [ do
        syncInputs st
        pure Noop
    ]
updateModel (EvalUpdate f) st = do
  let prev = f st
  let unload = #modelLoading .~ False :: Model -> Model
  let next = unload prev
  batchEff
    prev
    [ do
        when (modelLoading prev) $ do
          sink <- readMVar $ modelSink prev
          liftIO
            . void
            . spawnLink
            . deepseq (viewModel next)
            . sink
            . PushUpdate
            . PureUpdate
            $ unload
        longUri <- mkUri next
        Jsm.insertStorage ("cryptogram-" <> vsn) longUri
        pure Noop,
      do
        --
        -- NOTE : Workaround to fix slow rendering after screen switch.
        --
        sleepMilliSeconds 300
        pure SyncInputs
    ]
updateModel (PushUpdate value) st = do
  case value of
    PureUpdate f -> do
      let prev = f st
      prev <# do
        next <-
          handleAny
            ( \e -> do
                alert . from @String @Unicode $ displayException e
                pure $ #modelLoading .~ False
            )
            $ evalModel prev
        pure
          $ EvalUpdate next
    ImpureUpdate g ->
      st <# do
        f <- g
        pure . PushUpdate $ PureUpdate f
    EffectUpdate e ->
      st <# do
        e
        pure . PushUpdate $ PureUpdate id
    PureAndImpureUpdate f g -> do
      let prev = f st
      prev <# do
        next <- g
        pure . PushUpdate $ PureUpdate next
    PureAndEffectUpdate f e -> do
      let prev = f st
      prev <# do
        e
        pure . PushUpdate $ PureUpdate id

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
    (
      [ keyed "css-pre-theme" $ link_
          [ rel_ "stylesheet",
            href_ $ "miso-functora/pre-theme.min.css"
          ]
      ] <>
      ( if not (st ^. #modelState . #stEnableTheme)
          then mempty
          else
            [ keyed "css-theme" $ link_
                [ rel_ "stylesheet",
                  href_ $ "themes/" <> themeCssFile (st ^. #modelState . #stTheme)
                ]
            ]
      ) <>
      [ keyed "css-post-theme" $ link_
          [ rel_ "stylesheet",
            href_ $ "miso-functora/post-theme.min.css"
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
      [ keyed "css-pre-theme" $ link_
          [ rel_ "stylesheet",
            href_ "miso-functora/pre-theme.css"
          ]
      ] <>
      ( if not (st ^. #modelState . #stEnableTheme)
          then mempty
          else
            [ keyed "css-theme" $ link_
                [ rel_ "stylesheet",
                  href_ $ "themes/" <> themeCssFile (st ^. #modelState . #stTheme)
                ]
            ]
      ) <>
      [ keyed "css-post-theme" $ link_
          [ rel_ "stylesheet",
            href_ "miso-functora/post-theme.css"
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
  act <- JS.global ! ("document" :: Unicode) ! ("activeElement" :: Unicode)
  void . Syb.everywhereM (Syb.mkM $ fun act) $ modelState st
  where
    fun :: JS.JSVal -> Unique Unicode -> JSM (Unique Unicode)
    fun act txt = do
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
        elActive <- JS.strictEqual el act
        typ <- (el ! ("type" :: Unicode)) >>= JS.fromJSVal
        unless (elActive || typ == Just ("file" :: Unicode))
          $ el
          ^. JS.jss ("value" :: Unicode) (txt ^. #uniqueValue)
      pure txt

evalModel :: (MonadThrow m) => Model -> m (Model -> Model)
evalModel prev = pure $ const prev
