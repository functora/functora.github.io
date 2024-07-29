module App.Widgets.Templates
  ( newModel,
  )
where

import App.Types
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude
import Functora.Rates
import qualified Functora.Web as Web
import qualified Material.Snackbar as Snackbar

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
  defKm <-
    maybe
      (Aes.randomKm 32)
      pure
      (mSt ^? _Just . #modelState . #stKm)
  defIkm <-
    maybe
      (newPasswordField mempty)
      pure
      (mSt ^? _Just . #modelState . #stIkm)
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
  let defCpt =
        mSt ^? _Just . #modelState . #stCpt . _Just
  mApp <- unShareUri uri
  st <-
    maybe
      ( pure
          St
            { stKm = defKm,
              stIkm = defIkm,
              stDoc = defDoc,
              stPre = defPre,
              stScreen = defSc,
              stCpt = defCpt
            }
      )
      ( \st -> do
          if null $ st ^. #stKm . #kmIkm . #unIkm
            then pure st
            else case st ^. #stCpt of
              Nothing -> pure st
              Just cpt -> do
                bDoc :: ByteString <-
                  maybe
                    ( throwString
                        @MisoString
                        "Failed to decrypt the document!"
                    )
                    pure
                    $ Aes.unHmacDecrypt
                      ( Aes.drvSomeAesKey @Aes.Word256 $ st ^. #stKm
                      )
                      cpt
                doc <-
                  identityToUnique
                    =<< either (throwString . thd3) pure (decodeBinary bDoc)
                pure
                  $ st
                  & #stDoc
                  .~ doc
                  & (#stCpt :: Lens' (St Unique) (Maybe Aes.Crypto))
                  .~ Nothing
      )
      mApp
  pure
    Model
      { modelFav = Closed,
        modelMenu = Closed,
        modelLoading = True,
        modelState = st,
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
