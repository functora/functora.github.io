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
import qualified Functora.Prelude as Prelude
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
      st <- newModel Nothing uri
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
                finSt <- newModel (Just nextSt) uri
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
          el
            ^. JS.js1
              ("getElementsByTagName" :: MisoString)
              ("input" :: MisoString)
        inp <- inps !! 0
        act <-
          JS.global
            ! ("document" :: MisoString)
            ! ("activeElement" :: MisoString)
        elActive <- JS.strictEqual inp act
        unless elActive $ el ^. JS.jss ("value" :: MisoString) (txt ^. #uniqueValue)
      pure txt

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel st@Model {modelState = st0} = do
  km <-
    if (st0 ^. #stKm . #kmIkm . #unIkm == mempty)
      && (st0 ^. #stIkm . #fieldOutput == mempty)
      && isNothing (st0 ^. #stCpt)
      then Aes.randomKm 32
      else pure $ st0 ^. #stKm
  pure
    $ st
    & #modelState
    . #stKm
    .~ km

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
