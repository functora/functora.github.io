{-# LANGUAGE CPP #-}

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
    getCurrency,
    newModel,
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
import qualified App.Widgets.Templates as Templates
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Generics as Syb
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Money
import qualified Functora.Money as Money
import Functora.Prelude hiding (Field)
import Functora.Rates
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

getCurrency ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  MVar Market ->
  CurrencyCode ->
  m CurrencyInfo
getCurrency mark cur =
  withMarket mark $ do
    eInfo <- tryMarket $ getCurrencyInfo cur
    either (const . pure $ CurrencyInfo cur mempty) pure eInfo

--
-- TODO : simplify this !!!!
--
newModel ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Maybe (MVar Market) ->
  URI ->
  m Model
newModel mMark uri = do
  ct <- getCurrentTime
  prod <- liftIO newBroadcastTChanIO
  cons <- liftIO . atomically $ dupTChan prod
  market <- maybe newMarket pure mMark
  btc <- getCurrency market $ CurrencyCode "btc"
  usd <- getCurrency market $ CurrencyCode "usd"
  topMoney <- newMoney 0 btc
  bottomMoney <- newMoney 0 usd
  ikm <- newPasswordField mempty
  km <- Aes.randomKm 32
  mApp <- unShareUri uri
  defDoc <- liftIO Templates.invoiceTemplate
  defPre <- newDynamicTitleField mempty
  let defSc = Editor
  (sc, doc, pre, ext) <-
    maybe
      ( pure (defSc, defDoc, defPre, Nothing)
      )
      ( \ext -> do
          let sc = ext ^. #stExtScreen
          let pre = ext ^. #stExtPre
          if null $ ext ^. #stExtKm . #kmIkm . #unIkm
            then
              pure
                ( sc,
                  defDoc,
                  pre,
                  Just ext
                )
            else do
              bDoc :: ByteString <-
                maybe
                  ( throwString @Text "Failed to decrypt the document!"
                  )
                  pure
                  $ Aes.unHmacDecrypt
                    ( Aes.drvSomeAesKey @Aes.Word256 $ ext ^. #stExtKm
                    )
                    ( ext ^. #stExtDoc
                    )
              doc <-
                identityToUnique
                  =<< either (throwString . thd3) pure (decodeBinary bDoc)
              pure
                ( sc,
                  doc,
                  pre,
                  Nothing
                )
      )
      mApp
  let st =
        Model
          { modelHide = False,
            modelMenu = Closed,
            modelShare = Closed,
            modelTemplates = Closed,
            modelExamples = Closed,
            modelState =
              St
                { stScreen = sc,
                  stConv =
                    StConv
                      { stConvTopMoney = topMoney,
                        stConvBottomMoney = bottomMoney,
                        stConvTopOrBottom = Top
                      },
                  stDoc = doc,
                  stIkm = ikm,
                  stKm = km,
                  stPre = pre,
                  stExt = ext
                },
            modelMarket = market,
            modelCurrencies = [btc, usd],
            modelSnackbarQueue = Snackbar.initialQueue,
            modelProducerQueue = prod,
            modelConsumerQueue = cons,
            modelOnlineAt = ct
          }
  fmap (fromRight st) . tryMarket . withMarket market $ do
    currenciesInfo <- currenciesList <$> getCurrencies
    baseCur <-
      fmap (fromRight $ NonEmpty.head currenciesInfo)
        . tryMarket
        . getCurrencyInfo
        $ currencyInfoCode btc
    quoteCur <-
      fmap (fromRight $ NonEmpty.last currenciesInfo)
        . tryMarket
        . getCurrencyInfo
        $ currencyInfoCode usd
    let baseAmt =
          Tagged 1 ::
            Money.Money (Tags 'Signed |+| 'Base |+| 'MoneyAmount)
    quote <-
      getQuote
        (Funds baseAmt $ currencyInfoCode baseCur)
        $ currencyInfoCode quoteCur
    let quoteAmt = quoteMoneyAmount quote
    pure
      $ st
      --
      -- Converter
      --
      & #modelState
      . #stConv
      . #stConvTopMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged baseAmt)
      & #modelState
      . #stConv
      . #stConvTopMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged baseAmt
      & #modelState
      . #stConv
      . #stConvTopMoney
      . #moneyCurrency
      . #currencyOutput
      .~ baseCur
      & #modelState
      . #stConv
      . #stConvBottomMoney
      . #moneyAmount
      . #fieldInput
      . #uniqueValue
      .~ inspectRatioDef (unTagged quoteAmt)
      & #modelState
      . #stConv
      . #stConvBottomMoney
      . #moneyAmount
      . #fieldOutput
      .~ unTagged quoteAmt
      & #modelState
      . #stConv
      . #stConvBottomMoney
      . #moneyCurrency
      . #currencyOutput
      .~ quoteCur
      --
      -- Misc
      --
      & #modelCurrencies
      .~ currenciesInfo

newAssetAction :: Model -> ATraversal' Model [Asset Unique] -> Action
newAssetAction st optic =
  PushUpdate $ do
    cur <- getCurrency (st ^. #modelMarket) $ CurrencyCode "usd"
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

newPaymentMethodAction ::
  Model -> ATraversal' Model [PaymentMethod Unique] -> Action
newPaymentMethodAction st optic =
  PushUpdate $ do
    cur <- getCurrency (st ^. #modelMarket) $ CurrencyCode "btc"
    item <- newPaymentMethod cur $ Just mempty
    pure
      . ChanItem 0
      $ (textPopupPure @Text "Added payment!")
      . (& cloneTraversal optic %~ (<> [item]))

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
