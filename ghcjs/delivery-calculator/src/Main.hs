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
import qualified Data.Set as Set
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Money as Money
import qualified Functora.Rates as Rates
import qualified Functora.Web as Web
import Language.Javascript.JSaddle ((!))
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
    . handleAny
      ( \e -> do
          alert . from @String @Unicode $ displayException e
          sleepSeconds 5
      )
    $ do
      uri <- URI.mkURI . inspect =<< getCurrentURI
      mSt <- unShareUri uri
      web <- getWebOpts
      sink <- newEmptyMVar
      st <- newModel web sink Nothing uri
      startApp
        App
          { model = st,
            update = updateModel,
            Miso.view = viewModel,
            subs = mempty,
            events = Map.insert "focus" True defaultEvents,
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
updateModel (Tick f) st =
  f st <# do
    sleepSeconds 1
    ct <- getCurrentTime
    pure . Tick $ #modelTime .~ ct
updateModel (InitUpdate ext) prevSt = do
  effectSub prevSt $ \sink -> do
    liftIO . sink $ Tick id
    mvSink <- newMVar sink
    let nextSt = prevSt {modelSink = mvSink}
    if isJust ext
      then do
        liftIO
          . sink
          . PushUpdate
          . PureUpdate
          $ #modelLoading
          .~ False
        opfsSync sink nextSt
      else Jsm.selectStorage ("current-" <> vsn) $ \case
        Nothing -> do
          liftIO
            . sink
            . PushUpdate
            . PureUpdate
            $ #modelLoading
            .~ False
          opfsSync sink nextSt
        Just uri -> do
          finSt <- newModel (nextSt ^. #modelWebOpts) mvSink (Just nextSt) uri
          liftIO
            . sink
            . PushUpdate
            . PureUpdate
            $ ( const
                  $ finSt
                  & #modelLoading
                  .~ False
              )
          opfsSync sink finSt
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
updateModel (LinkUpdate f) st =
  noEff $ f st
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
        uri <- stUri next
        Jsm.insertStorage ("current-" <> vsn) uri
        syncUri uri
        nextUri <- stUri $ next & #modelState . #stScreen %~ unQrCode
        uriViewer <-
          newFieldPair mempty
            . DynamicFieldText
            . from @String @Unicode
            $ URI.renderStr nextUri
        pure
          . LinkUpdate
          $ #modelUriViewer
          %~ mergeFieldPairs
            [ uriViewer
                & #fieldPairValue
                . #fieldOpts
                . #fieldOptsAllowCopy
                .~ True
                & #fieldPairValue
                . #fieldType
                .~ FieldTypeQrCode
            ],
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
            href_ $ "miso-functora/pre-theme.css"
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
        rateValue <-
          fmap
            ( either
                ( const
                    $ prev
                    ^. #modelState
                    . #stExchangeRate
                    . #fieldOutput
                )
                ( ^.
                    #quoteMoneyAmount
                      . to unTagged
                )
            )
            . Rates.tryMarket
            . Rates.getQuote web base
            $ prev
            ^. #modelState
            . #stMerchantCurrency
            . #currencyOutput
            . #currencyInfoCode
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

opfsSync :: (Action -> IO ()) -> Model -> JSM ()
opfsSync sink st = do
  opfsRead sink st
  opfsFree st

opfsRead :: (Action -> IO ()) -> Model -> JSM ()
opfsRead sink st =
  forM_ (zip [0 ..] assets) . uncurry $ \assetIdx asset -> do
    let fields = fmap (^. #fieldPairValue) $ asset ^. #assetFieldPairs
    forM_ (zip [0 ..] fields) . uncurry $ \fieldIdx field -> do
      let optic =
            #modelState
              . #stAssets
              . ix assetIdx
              . #assetFieldPairs
              . ix fieldIdx
              . #fieldPairValue
      when
        ( (field ^. #fieldType == FieldTypeImage)
            && isJust (field ^. #fieldBlobOpts . #blobOptsOpfsDir)
            && isJust (field ^. #fieldBlobOpts . #blobOptsOpfsFile)
        )
        $ Jsm.opfsRead (fieldBlobOpts field)
        . flip whenJust
        $ \uri ->
          liftIO
            . sink
            . PushUpdate
            . PureUpdate
            $ ( cloneTraversal optic
                  . #fieldInput
                  . #uniqueValue
                  .~ uri
              )
            . ( cloneTraversal optic
                  . #fieldOutput
                  .~ DynamicFieldText uri
              )
  where
    assets = st ^. #modelState . #stAssets

--
-- NOTE : test with
--
-- const root = await navigator.storage.getDirectory(); for await (let name of root.keys()) {console.log(name);} const dir = await root.getDirectoryHandle("delivery-calculator-images"); for await (let name of dir.keys()) {console.log(name);}
--
opfsFree :: Model -> JSM ()
opfsFree st =
  forM_ groups $ \case
    items@((dir, _) : _) ->
      Jsm.opfsList dir $ \case
        Nothing -> pure ()
        Just opfsFiles -> do
          let modelFiles = fromList $ fmap snd items
          forM_ opfsFiles $ \file ->
            when (Set.notMember file modelFiles)
              $ Jsm.opfsRemove
                defBlobOpts
                  { blobOptsOpfsDir = Just dir,
                    blobOptsOpfsFile = Just file
                  }
    _ ->
      pure ()
  where
    groups :: [[(Unicode, Unicode)]]
    groups =
      groupAllOn fst
        . catMaybes
        . map
          ( \x ->
              (,)
                <$> (x ^. #fieldBlobOpts . #blobOptsOpfsDir)
                <*> (x ^. #fieldBlobOpts . #blobOptsOpfsFile)
          )
        . Syb.listify (\(_ :: Field DynamicField Unique) -> True)
        $ modelState st
