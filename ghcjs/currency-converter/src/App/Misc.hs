module App.Misc
  ( getConverterAmountOptic,
    getConverterCurrencyOptic,
    pushActionQueue,
    onKeyDownAction,
    copyIntoClipboard,
    snackbarClosed,
    drainTChan,
    verifyUid,
    forceRender,
    duplicateAt,
    removeAt,
    getSomeCurrency,
    newAssetAction,
    newTextPropAction,
    newPaymentMethodAction,
  )
where

import App.Types
import Functora.Money
import Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Snackbar as Snackbar
import Miso hiding (view)

getConverterAmountOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Amount Unique)
getConverterAmountOptic = \case
  Top -> #modelState . #stateTopMoney . #moneyAmount
  Bottom -> #modelState . #stateBottomMoney . #moneyAmount

getConverterCurrencyOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Currency Unique)
getConverterCurrencyOptic = \case
  Top -> #modelState . #stateTopMoney . #moneyCurrency
  Bottom -> #modelState . #stateBottomMoney . #moneyCurrency

pushActionQueue :: (MonadIO m) => Model -> ChanItem (Model -> Model) -> m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)

onKeyDownAction :: Uid -> KeyCode -> Action
onKeyDownAction uid (KeyCode code) =
  PushUpdate $ do
    verifyUid uid
    let enterOrEscape = [13, 27] :: [Int]
    when (code `elem` enterOrEscape)
      . void
      . JS.eval @Text
      $ "document.getElementById('"
      <> htmlUid uid
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

verifyUid :: Uid -> JSM ()
verifyUid uid =
  when (nullUid uid)
    $ consoleLog "UNEXPECTED NULL UID"

forceRender :: Model -> JSM ()
forceRender st =
  void . spawnLink $ do
    sleepMilliSeconds 300
    pushActionQueue st $ ChanItem 0 id

duplicateAt ::
  ( Data a
  ) =>
  Model ->
  ATraversal' Model [a] ->
  Int ->
  Action
duplicateAt st optic idx =
  PushUpdate $ do
    duplicator <- newUniqueDuplicator @Text
    let updater loc el =
          if loc == idx
            then [el, duplicator el]
            else [el]
    --
    -- TODO : maybe move to general update function?
    -- Probably overhead is not to big,
    -- and this will cover all corner cases everywhere.
    --
    forceRender st
    pure
      . ChanItem 0
      $ (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

removeAt ::
  Model ->
  ATraversal' Model [a] ->
  Int ->
  Action
removeAt st optic idx =
  PushUpdate $ do
    let updater loc el =
          if loc == idx
            then mempty
            else [el]
    forceRender st
    pure
      . ChanItem 0
      $ (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

getSomeCurrency :: Model -> CurrencyCode -> CurrencyInfo
getSomeCurrency st cur =
  fromMaybe (head currencies) $ find ((== cur) . currencyInfoCode) currencies
  where
    currencies = st ^. #modelCurrencies

newAssetAction :: Model -> ATraversal' Model [Asset Unique] -> Action
newAssetAction st optic =
  PushUpdate $ do
    item <- newAsset mempty 0 . getSomeCurrency st $ CurrencyCode "usd"
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (item :))

newTextPropAction :: ATraversal' Model [TextProp Unique] -> Action
newTextPropAction optic =
  PushUpdate $ do
    item <- newTextProp mempty mempty
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (item :))

newPaymentMethodAction ::
  Model -> ATraversal' Model [PaymentMethod Unique] -> Action
newPaymentMethodAction st optic =
  PushUpdate $ do
    item <- newPaymentMethod 0 . getSomeCurrency st $ CurrencyCode "btc"
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (item :))
