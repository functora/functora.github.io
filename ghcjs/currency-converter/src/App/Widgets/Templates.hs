module App.Widgets.Templates
  ( newStDoc,
    newModel,
  )
where

import App.Types
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Money hiding (Currency, Money, Text)
import Functora.Rates
import qualified Functora.Web as Web
import qualified Material.Snackbar as Snackbar

newStDoc :: IO (StDoc Unique)
newStDoc = do
  conv <- newStConv
  pfav <- newTextField mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocPreFavName = pfav,
        stDocFieldPairs = mempty,
        stDocOnlineOrOffline = Online
      }

newStConv :: (MonadThrow m, MonadUnliftIO m) => m (StConv Unique)
newStConv = do
  ct <- getCurrentTime
  topMoney <- newMoney 1 btc
  bottomMoney <- newMoney 0 usd
  pure
    StConv
      { stConvTopMoney = topMoney,
        stConvBottomMoney = bottomMoney,
        stConvTopOrBottom = Top,
        stConvCreatedAt = ct
      }

newModel ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Web.Opts ->
  Maybe Model ->
  URI ->
  m Model
newModel webOpts mSt uri = do
  ct <- getCurrentTime
  prod <- liftIO newBroadcastTChanIO
  cons <- liftIO . atomically $ dupTChan prod
  market <-
    maybe
      newMarket
      pure
      (mSt ^? _Just . #modelMarket)
  ikm <-
    maybe
      (newPasswordField mempty)
      pure
      (mSt ^? _Just . #modelState . #stIkm)
  km <-
    maybe
      (Aes.randomKm 32)
      pure
      (mSt ^? _Just . #modelState . #stKm)
  defDoc <-
    maybe
      (liftIO newStDoc)
      pure
      (mSt ^? _Just . #modelState . #stDoc)
  defPre <-
    maybe
      (newDynamicTitleField mempty)
      pure
      (mSt ^? _Just . #modelState . #stPre)
  let defSc =
        fromMaybe
          Converter
          (mSt ^? _Just . #modelState . #stScreen)
  let defExt =
        mSt ^? _Just . #modelState . #stExt . _Just
  mApp <- unShareUri uri
  (sc, doc, pre, ext) <-
    maybe
      ( pure (defSc, defDoc, defPre, defExt)
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
                  ( throwString @MisoString "Failed to decrypt the document!"
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
                  defExt
                )
      )
      mApp
  pure
    Model
      { modelFav = Closed,
        modelMenu = Closed,
        modelLoading = True,
        modelState =
          St
            { stScreen = sc,
              stDoc = doc,
              stIkm = ikm,
              stKm = km,
              stPre = pre,
              stExt = ext
            },
        modelMarket = market,
        modelFavMap = mempty,
        modelCurrencies =
          fromMaybe
            [btc, usd]
            (mSt ^? _Just . #modelCurrencies),
        modelSnackbarQueue =
          fromMaybe
            Snackbar.initialQueue
            (mSt ^? _Just . #modelSnackbarQueue),
        modelProducerQueue = prod,
        modelConsumerQueue = cons,
        modelOnlineAt = fromMaybe ct (mSt ^? _Just . #modelOnlineAt),
        modelWebOpts = webOpts
      }

--
-- Const
--

usd :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "usd") mempty

btc :: CurrencyInfo
btc = CurrencyInfo (CurrencyCode "btc") mempty
