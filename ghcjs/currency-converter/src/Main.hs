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
import App.AmountWidget
import App.CurrencyWidget
import qualified App.Misc as Misc
import App.Types
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Version as Version
import Functora.Money
import Functora.Prelude as Prelude
import Functora.Rates
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (view)
import qualified Miso
import Miso.String hiding (cons, foldl, intercalate, null, reverse)
import qualified Paths_app as Paths

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

syncInputs :: Model -> JSM ()
syncInputs st =
  --
  -- NOTE : The "correct" way is to use "controlled input" with
  -- TextField.setValue but it does cause race conditions
  -- when user types "too fast":
  --
  -- https://github.com/dmjio/miso/issues/272
  --
  forM_ enumerate $ \loc -> do
    let moneyLens = Misc.getMoneyOptic loc
    JS.eval @Text
      $ "var el = document.getElementById('"
      <> htmlUuid (st ^. cloneLens moneyLens . #modelMoneyAmountUuid)
      <> "'); if (el && !(el.getElementsByTagName('input')[0] === document.activeElement)) el.value = '"
      <> (st ^. cloneLens moneyLens . #modelMoneyAmountInput)
      <> "';"

evalModel :: (MonadThrow m, MonadUnliftIO m) => Model -> m Model
evalModel st = do
  let loc = st ^. #modelData . #modelDataTopOrBottom
  let baseLens = getBaseMoneyOptic loc
  let quoteLens = getQuoteMoneyOptic loc
  let baseAmtInput =
        case st ^. cloneLens baseLens . #modelMoneyAmountInput of
          amt
            | null amt ->
                inspectMoneyAmount
                  $ st
                  ^. cloneLens baseLens
                  . #modelMoneyAmountOutput
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
                . #modelMoneyCurrencyInfo
                . #currencyInfoCode
        quote <-
          getQuote funds
            $ st
            ^. cloneLens quoteLens
            . #modelMoneyCurrencyInfo
            . #currencyInfoCode
        let quoteAmt = quoteMoneyAmount quote
        ct <- getCurrentTime
        pure
          $ st
          & cloneLens baseLens
          . #modelMoneyAmountInput
          .~ baseAmtInput
          & cloneLens baseLens
          . #modelMoneyAmountOutput
          .~ unTag @'Base baseAmt
          & cloneLens quoteLens
          . #modelMoneyAmountInput
          .~ inspectMoneyAmount quoteAmt
          & cloneLens quoteLens
          . #modelMoneyAmountOutput
          .~ unTag @'Quote quoteAmt
          & #modelUpdatedAt
          .~ ct

getBaseMoneyOptic :: TopOrBottom -> ALens' Model ModelMoney
getBaseMoneyOptic = \case
  Top -> #modelData . #modelDataTopMoney
  Bottom -> #modelData . #modelDataBottomMoney

getQuoteMoneyOptic :: TopOrBottom -> ALens' Model ModelMoney
getQuoteMoneyOptic = \case
  Top -> #modelData . #modelDataBottomMoney
  Bottom -> #modelData . #modelDataTopMoney

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

mainWidget :: Model -> View Action
mainWidget st =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    $ [ LayoutGrid.inner
          ( [ class_ "container"
            ]
              --
              -- NOTE : Hiding widget on the first render to avoid flickering.
              --
              <> ( if st ^. #modelHide
                    then [style_ [("display", "none")]]
                    else mempty
                 )
          )
          ( screenWidget st
              <> [
                   -- LayoutGrid.cell [LayoutGrid.span12]
                   --   . (: mempty)
                   --   $ div_ mempty [inspect $ st ^. #modelData],
                   swapScreenWidget st,
                   tosWidget,
                   Snackbar.snackbar (Snackbar.config Misc.snackbarClosed)
                    $ modelSnackbarQueue st
                 ]
          )
      ]
    <> ( if st ^. #modelHide
          then [div_ [class_ "lds-dual-ring"] mempty]
          else mempty
       )

screenWidget :: Model -> [View Action]
screenWidget st@Model {modelScreen = Converter} =
  [ amountWidget st Top,
    currencyWidget st $ Misc.getMoneyOptic Top,
    amountWidget st Bottom,
    currencyWidget st $ Misc.getMoneyOptic Bottom,
    swapAmountsWidget,
    swapCurrenciesWidget
  ]
screenWidget st@Model {modelScreen = InvoiceEditor} =
  [ amountWidget st Top,
    currencyWidget st $ Misc.getMoneyOptic Top,
    currencyWidget st
      $ #modelData
      . #modelDataPaymentMethodsInput
      . #paymentMethodMoney
  ]

swapScreenWidget :: Model -> View Action
swapScreenWidget st =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span8Tablet,
      LayoutGrid.span4Phone
    ]
    . (: mempty)
    . Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
    $ case st ^. #modelScreen of
      Converter -> "Create invoice"
      InvoiceEditor -> "Show converter"
  where
    onClickAction =
      PushUpdate
        ( do
            --
            -- NOTE : Need to sync text inputs on new screen.
            --
            sleepMilliSeconds 300
            Misc.pushActionQueue st $ ChanItem 0 id
        )
        $ ChanItem
          0
          ( &
              #modelScreen
                %~ ( \case
                      Converter -> InvoiceEditor
                      InvoiceEditor -> Converter
                   )
          )

tosWidget :: View Action
tosWidget =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.subtitle2,
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text "\169 2024 Functora. All rights reserved. ",
      Miso.text "By continuing to use this software, you agree to the ",
      a_ [href_ "license.html"] [Miso.text "Terms of Service"],
      Miso.text " and ",
      a_ [href_ "privacy.html"] [Miso.text "Privacy Policy"],
      Miso.text ". ",
      Miso.text . ms $ "Version " <> vsn <> "."
    ]

upToDate :: UTCTime -> UTCTime -> Bool
upToDate lhs rhs =
  diff < 3600
  where
    diff = abs . toRational $ diffUTCTime lhs rhs

vsn :: Text
vsn =
  T.intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version
