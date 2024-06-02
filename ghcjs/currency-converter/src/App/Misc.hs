{-# LANGUAGE CPP #-}

module App.Misc
  ( getConverterAmountOptic,
    getConverterCurrencyOptic,
    pushActionQueue,
    onKeyDownAction,
    copyIntoClipboard,
    textPopup,
    textPopupPure,
    textPopupClosed,
    drainTChan,
    verifyUid,
    duplicateAt,
    removeAt,
    moveUp,
    moveDown,
    getSomeCurrency,
    newAssetAction,
    newFieldPairAction,
    newPaymentMethodAction,
    stUri,
    stExtUri,
    setScreenPure,
    setScreenAction,
    setExtScreenAction,
    vsn,
  )
where

import App.Types
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Money
import Functora.Prelude hiding (Field)
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Snackbar as Snackbar
import Miso hiding (URI, view)
import qualified Paths_app as Paths
import qualified Text.URI as URI
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
    success <- JS.function $ \_ _ _ -> textPopup st $ "Copied " <> txt
    failure <- JS.function $ \_ _ _ -> textPopup st $ "Failed to copy " <> txt
    void $ prom ^. JS.js2 ("then" :: Text) success failure

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

textPopupPure :: (Show a, Data a) => Model -> a -> Model
textPopupPure st x =
  st
    & #modelSnackbarQueue
    %~ (Snackbar.addMessage msg . Snackbar.clearQueue)
  where
    msg =
      inspect x
        & Snackbar.message
        & Snackbar.setActionIcon (Just (Snackbar.icon "close"))
        & Snackbar.setOnActionIconClick textPopupClosed

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
            then [el, duplicator el]
            else [el]
    pure
      . ChanItem 0
      $ (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

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
      $ (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

moveUp :: ATraversal' Model [a] -> Int -> Action
moveUp optic idx =
  PushUpdate
    . pure
    . ChanItem 0
    $ (& cloneTraversal optic %~ swapAt (idx - 1) idx)

moveDown :: ATraversal' Model [a] -> Int -> Action
moveDown optic idx =
  PushUpdate
    . pure
    . ChanItem 0
    $ (& cloneTraversal optic %~ swapAt idx (idx + 1))

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

getSomeCurrency :: Model -> CurrencyCode -> CurrencyInfo
getSomeCurrency st cur =
  fromMaybe (head currencies) $ find ((== cur) . currencyInfoCode) currencies
  where
    currencies = st ^. #modelCurrencies

newAssetAction :: Model -> ATraversal' Model [Asset Unique] -> Action
newAssetAction st optic =
  PushUpdate $ do
    item <- newAsset mempty mempty 0 . getSomeCurrency st $ CurrencyCode "usd"
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (<> [item]))

newFieldPairAction ::
  ATraversal' Model [FieldPair DynamicField Unique] -> Action
newFieldPairAction optic =
  PushUpdate $ do
    item <- newFieldPair mempty $ DynamicFieldText mempty
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (<> [item]))

newPaymentMethodAction ::
  Model -> ATraversal' Model [PaymentMethod Unique] -> Action
newPaymentMethodAction st optic =
  PushUpdate $ do
    item <- newPaymentMethod 0 . getSomeCurrency st $ CurrencyCode "btc"
    pure . ChanItem 0 $ (& cloneTraversal optic %~ (<> [item]))

stQuery :: (MonadThrow m) => St Identity -> m [URI.QueryParam]
stQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      . Aes.encryptHmac aes
      $ encodeBinary (st ^. #stDoc)
  kKm <- URI.mkQueryKey "k"
  vKm <-
    (URI.mkQueryValue <=< encodeText)
      . encodeBinary
      . fromEither
      $ fmap (& #kmIkm .~ Ikm mempty) ekm
  kSc <- URI.mkQueryKey "s"
  vSc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stScreen)
  kPre <- URI.mkQueryKey "p"
  vPre <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stPre)
  pure
    [ URI.QueryParam kDoc vDoc,
      URI.QueryParam kKm vKm,
      URI.QueryParam kSc vSc,
      URI.QueryParam kPre vPre
    ]
  where
    aes :: Aes.SomeAesKey
    aes = Aes.drvSomeAesKey @Aes.Word256 $ fromEither ekm
    ekm :: Either Aes.Km Aes.Km
    ekm =
      case st ^. #stIkm . #fieldOutput of
        ikm | null ikm -> Left (st ^. #stKm)
        ikm -> Right $ (st ^. #stKm) & #kmIkm .~ Ikm (encodeUtf8 ikm)
    encodeText :: (MonadThrow m) => BL.ByteString -> m Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stExtQuery :: (MonadThrow m) => StExt Identity -> m [URI.QueryParam]
stExtQuery st = do
  kDoc <- URI.mkQueryKey "d"
  vDoc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtDoc)
  kKm <- URI.mkQueryKey "k"
  vKm <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtKm)
  kSc <- URI.mkQueryKey "s"
  vSc <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtScreen)
  kPre <- URI.mkQueryKey "p"
  vPre <-
    (URI.mkQueryValue <=< encodeText)
      $ encodeBinary (st ^. #stExtPre)
  pure
    [ URI.QueryParam kDoc vDoc,
      URI.QueryParam kKm vKm,
      URI.QueryParam kSc vSc,
      URI.QueryParam kPre vPre
    ]
  where
    encodeText :: (MonadThrow m) => BL.ByteString -> m Text
    encodeText =
      either throw pure
        . decodeUtf8'
        . B64URL.encode
        . from @BL.ByteString @ByteString

stUri :: (MonadThrow m) => Model -> m URI
stUri st = do
  uri <- mkURI baseUri
  qxs <- stQuery . uniqueToIdentity $ st ^. #modelState
  pure
    $ uri
      { URI.uriQuery = qxs
      }

stExtUri :: (MonadThrow m) => StExt Unique -> m URI
stExtUri st = do
  uri <- mkURI baseUri
  qxs <- stExtQuery $ uniqueToIdentity st
  pure
    $ uri
      { URI.uriQuery = qxs
      }

baseUri :: Text
#ifdef GHCID
baseUri =
  "http://localhost:8080"
#else
baseUri =
  "https://functora.github.io/apps/currency-converter/" <> vsn <> "/index.html"
#endif

setScreenPure :: Screen -> Model -> Model
setScreenPure sc =
  (& #modelState . #stScreen .~ sc)

setScreenAction :: Screen -> Action
setScreenAction =
  pureUpdate 0 . setScreenPure

setExtScreenAction :: Screen -> Action
setExtScreenAction sc =
  pureUpdate 0 (& #modelState . #stExt . _Just . #stExtScreen .~ sc)

vsn :: Text
vsn =
  T.intercalate "."
    . fmap inspect
    $ Version.versionBranch Paths.version
