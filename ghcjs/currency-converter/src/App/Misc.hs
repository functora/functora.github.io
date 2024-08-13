module App.Misc
  ( getConverterAmountOptic,
    getConverterCurrencyOptic,
    pushActionQueue,
    onKeyDownAction,
    drainTChan,
    verifyUid,
    openBrowserPageAction,
    browserLink,
  )
where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Text.URI as URI

getConverterAmountOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Field Rational Unique)
getConverterAmountOptic = \case
  Top -> #modelState . #stDoc . #stDocTopMoney . #moneyAmount
  Bottom -> #modelState . #stDoc . #stDocBottomMoney . #moneyAmount

getConverterCurrencyOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Currency Unique)
getConverterCurrencyOptic = \case
  Top -> #modelState . #stDoc . #stDocTopMoney . #moneyCurrency
  Bottom -> #modelState . #stDoc . #stDocBottomMoney . #moneyCurrency

pushActionQueue :: (MonadIO m) => Model -> ChanItem (Model -> Model) -> m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)

onKeyDownAction :: Uid -> KeyCode -> JSM (model -> model)
onKeyDownAction uid (KeyCode code) = do
  verifyUid uid
  let enterOrEscape = [13, 27] :: [Int]
  when (code `elem` enterOrEscape)
    . void
    . JS.eval @MisoString
    $ "document.getElementById('"
    <> htmlUid uid
    <> "').getElementsByTagName('input')[0].blur();"
  pure id

drainTChan :: (MonadIO m) => TChan (ChanItem a) -> m [a]
drainTChan chan = do
  item <- liftIO . atomically $ readTChan chan
  liftIO
    . fmap ((chanItemValue item :) . reverse)
    . drainInto []
    $ chanItemDelay item
  where
    drainInto acc delay = do
      item <- atomically $ tryReadTChan chan
      case item of
        Nothing | delay == 0 -> pure acc
        Nothing -> do
          sleepMilliSeconds $ from @Natural @Integer delay
          drainInto acc 0
        Just next ->
          drainInto (chanItemValue next : acc)
            . max delay
            $ chanItemDelay next

verifyUid :: Uid -> JSM ()
verifyUid uid =
  when (nullUid uid)
    $ consoleLog @MisoString "UNEXPECTED NULL UID"

openBrowserPageAction :: URI -> Action
openBrowserPageAction uri =
  PushUpdate $ do
    void $ JS.global ^. JS.js1 @MisoString "openBrowserPage" (URI.render uri)
    pure $ ChanItem 0 id

browserLink :: Prelude.Text -> MisoString -> View Action
browserLink uri txt =
  a_
    [ href_ "#!",
      onClick
        . openBrowserPageAction
        . either impureThrow id
        $ URI.mkURI uri
    ]
    [ text txt
    ]
