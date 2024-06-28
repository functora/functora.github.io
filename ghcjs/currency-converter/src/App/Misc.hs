module App.Misc
  ( getConverterAmountOptic,
    getConverterCurrencyOptic,
    pushActionQueue,
    onKeyDownAction,
    copyIntoClipboard,
    copyIntoClipboardAction,
    textPopup,
    textPopupPure,
    textPopupClear,
    textPopupClosed,
    drainTChan,
    verifyUid,
    duplicateAt,
    removeAt,
    moveUp,
    moveDown,
    newAssetAction,
    newFieldPairAction,
    newPaymentMethodAction,
  )
where

import App.Types
import qualified Data.Generics as Syb
import Functora.Money
import Functora.Prelude hiding (Field)
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Snackbar as Snackbar
import Miso hiding (URI, view)
import qualified Prelude

getConverterAmountOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Field Rational Unique)
getConverterAmountOptic = \case
  Top -> #modelState . #stConv . #stConvTopMoney . #moneyAmount
  Bottom -> #modelState . #stConv . #stConvBottomMoney . #moneyAmount

getConverterCurrencyOptic ::
  ( Functor f
  ) =>
  TopOrBottom ->
  LensLike' f Model (Currency Unique)
getConverterCurrencyOptic = \case
  Top -> #modelState . #stConv . #stConvTopMoney . #moneyCurrency
  Bottom -> #modelState . #stConv . #stConvBottomMoney . #moneyCurrency

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
    success <- JS.function $ \_ _ _ -> textPopup @Text st "Copied!"
    failure <- JS.function $ \_ _ _ -> textPopup @Text st "Failed to copy!"
    void $ prom ^. JS.js2 ("then" :: Text) success failure

copyIntoClipboardAction :: (Show a, Data a) => Model -> a -> Action
copyIntoClipboardAction st x =
  PushUpdate $ do
    copyIntoClipboard st x
    pure $ ChanItem 0 id

textPopup :: (Show a, Data a) => Model -> a -> JSM ()
textPopup st x =
  pushActionQueue st
    $ ChanItem
      0
      ( &
          #modelSnackbarQueue
            %~ (Snackbar.addMessage msg . Snackbar.clearQueue)
      )
  where
    msg =
      inspect x
        & Snackbar.message
        & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
        & Snackbar.setOnActionIconClick textPopupClosed

textPopupPure :: (Show a, Data a) => a -> Model -> Model
textPopupPure x st =
  st
    & #modelSnackbarQueue
    %~ (Snackbar.addMessage msg . Snackbar.clearQueue)
  where
    msg =
      inspect x
        & Snackbar.message
        & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
        & Snackbar.setOnActionIconClick textPopupClosed

textPopupClear :: Model -> Model
textPopupClear =
  (& #modelSnackbarQueue %~ Snackbar.clearQueue)

textPopupClosed :: Snackbar.MessageId -> Action
textPopupClosed msg =
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

duplicateAt ::
  forall a.
  ( Data a
  ) =>
  ATraversal' Model [a] ->
  Int ->
  Action
duplicateAt optic idx =
  PushUpdate $ do
    duplicator <- newUniqueDuplicator @Text
    let updater loc el =
          if loc == idx
            then [el, closed $ duplicator el]
            else [el]
    pure
      . ChanItem 0
      $ (textPopupPure $ "Duplicated #" <> inspect @Text (idx + 1) <> "!")
      . (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))
  where
    closed :: a -> a
    closed = Syb.everywhere $ Syb.mkT $ const Closed

removeAt ::
  ATraversal' Model [a] ->
  Int ->
  Action
removeAt optic idx =
  PushUpdate $ do
    let updater loc el =
          if loc == idx
            then mempty
            else [el]
    pure
      . ChanItem 0
      $ (textPopupPure $ "Removed #" <> inspect @Text (idx + 1) <> "!")
      . (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

moveUp :: ATraversal' Model [a] -> Int -> Action
moveUp optic idx =
  PushUpdate
    . pure
    . ChanItem 0
    $ (textPopupPure $ "Moved #" <> inspect @Text (idx + 1) <> " up!")
    . (& cloneTraversal optic %~ swapAt (idx - 1) idx)

moveDown :: ATraversal' Model [a] -> Int -> Action
moveDown optic idx =
  PushUpdate
    . pure
    . ChanItem 0
    $ (textPopupPure $ "Moved #" <> inspect @Text (idx + 1) <> " down!")
    . (& cloneTraversal optic %~ swapAt idx (idx + 1))

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs
  | i == j = xs
  | i < 0 || i >= len = xs
  | j < 0 || j >= len = xs
  | otherwise = do
      (idx, val) <- zip [0 ..] xs
      pure
        $ if
          | idx == i -> jval
          | idx == j -> ival
          | otherwise -> val
  where
    len = length xs
    ival = xs Prelude.!! i
    jval = xs Prelude.!! j

newAssetAction :: ATraversal' Model [Asset Unique] -> Action
newAssetAction optic =
  PushUpdate $ do
    let cur = CurrencyInfo (CurrencyCode "usd") mempty
    item <- newAsset "Price" 0 cur
    pure
      . ChanItem 0
      $ (textPopupPure @Text "Added asset!")
      . (& cloneTraversal optic %~ (<> [item]))

newFieldPairAction ::
  ATraversal' Model [FieldPair DynamicField Unique] -> Action
newFieldPairAction optic =
  PushUpdate $ do
    item <- newFieldPair mempty $ DynamicFieldText mempty
    pure
      . ChanItem 0
      $ (textPopupPure @Text "Added details!")
      . (& cloneTraversal optic %~ (<> [item]))

newPaymentMethodAction :: ATraversal' Model [PaymentMethod Unique] -> Action
newPaymentMethodAction optic =
  PushUpdate $ do
    let cur = CurrencyInfo (CurrencyCode "btc") mempty
    item <- newPaymentMethod cur $ Just mempty
    pure
      . ChanItem 0
      $ (textPopupPure @Text "Added payment!")
      . (& cloneTraversal optic %~ (<> [item]))
