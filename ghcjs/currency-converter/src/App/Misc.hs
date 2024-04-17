module App.Misc
  ( getConverterAmountLens,
    getConverterCurrencyLens,
    pushActionQueue,
    onKeyDownAction,
    copyIntoClipboard,
    snackbarClosed,
    drainTChan,
  )
where

import App.Types
import Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Snackbar as Snackbar
import Miso hiding (view)

getConverterAmountLens :: TopOrBottom -> ALens' Model (Amount Unique)
getConverterAmountLens = \case
  Top -> #modelState . #stateTopMoney . #moneyAmount
  Bottom -> #modelState . #stateBottomMoney . #moneyAmount

getConverterCurrencyLens :: TopOrBottom -> ALens' Model (Currency Unique)
getConverterCurrencyLens = \case
  Top -> #modelState . #stateTopMoney . #moneyCurrency
  Bottom -> #modelState . #stateBottomMoney . #moneyCurrency

pushActionQueue :: (MonadIO m) => Model -> ChanItem (Model -> Model) -> m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)

onKeyDownAction :: UUID -> KeyCode -> Action
onKeyDownAction uuid (KeyCode code) =
  PushUpdate $ do
    let enterOrEscape = [13, 27] :: [Int]
    when (code `elem` enterOrEscape)
      . void
      . JS.eval @Text
      $ "document.getElementById('"
      <> htmlUuid uuid
      <> "').getElementsByTagName('input')[0].blur();"
    pure
      $ ChanItem 300 id

copyIntoClipboard :: (Show a, Data a) => Model -> a -> JSM ()
copyIntoClipboard st x = do
  let txt = inspect @Text x
  unless (null txt) $ do
    clip <- JS.global JS.! ("navigator" :: Text) JS.! ("clipboard" :: Text)
    prom <- clip ^. JS.js1 ("writeText" :: Text) txt
    success <- JS.function $ \_ _ _ -> push $ "Copied " <> txt
    failure <- JS.function $ \_ _ _ -> push $ "Failed to copy " <> txt
    void $ prom ^. JS.js2 ("then" :: Text) success failure
  where
    push =
      pushActionQueue st
        . updateSnackbar Snackbar.clearQueue

updateSnackbar ::
  ( Show a,
    Data a
  ) =>
  ( Snackbar.Queue Action -> Snackbar.Queue Action
  ) ->
  a ->
  ChanItem (Model -> Model)
updateSnackbar f x =
  ChanItem 0 (& #modelSnackbarQueue %~ (Snackbar.addMessage msg . f))
  where
    msg =
      inspect x
        & Snackbar.message
        & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
        & Snackbar.setOnActionIconClick snackbarClosed

snackbarClosed :: Snackbar.MessageId -> Action
snackbarClosed msg =
  pureUpdate 0 (& #modelSnackbarQueue %~ Snackbar.close msg)

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
