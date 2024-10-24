module App.Widgets.Templates
  ( newModel,
    newDonateDoc,
  )
where

import App.Types
import qualified App.Widgets.Bolt11 as B11
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Miso.Prelude

newModel :: (MonadThrow m, MonadUnliftIO m) => Maybe Model -> URI -> m Model
newModel mSt uri = do
  prod <- liftIO newBroadcastTChanIO
  cons <- liftIO . atomically $ dupTChan prod
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
  favName <- newTextField mempty
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
                        @String
                        "Failed to decrypt the document!"
                    )
                    pure
                    $ Aes.unHmacDecrypt
                      ( Aes.drvSomeAesKey @Aes.Word256 $ st ^. #stKm
                      )
                      cpt
                doc <-
                  identityToUnique
                    =<< either
                      (throwString . thd3)
                      ( \doc ->
                          pure
                            $ B11.mergeBolt11Viewers
                              (B11.makeBolt11Viewer doc)
                              doc
                      )
                      (decodeBinary bDoc)
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
        modelLinks = Closed,
        modelLoading = True,
        modelState = st,
        modelFavMap = mempty,
        modelFavName = favName,
        modelUriViewer = mempty,
        modelProducerQueue = prod,
        modelConsumerQueue = cons
      }

newDonateDoc :: IO (StDoc Unique)
newDonateDoc = do
  doc <- newStDoc
  title <- newFieldPair mempty $ DynamicFieldText "Hello, User!"
  message <- newFieldPair mempty $ DynamicFieldText exampleDonationText
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText exampleBtcAddress
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText exampleXmrAddress
  pure
    doc
      { stDocFieldPairs =
          [ title
              & #fieldPairValue
              . #fieldType
              .~ FieldTypeTitle,
            message,
            qr btcMtd,
            qr xmrMtd
          ]
      }
  where
    qr :: FieldPair a b -> FieldPair a b
    qr = (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)

exampleBtcAddress :: Unicode
exampleBtcAddress = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q"

exampleXmrAddress :: Unicode
exampleXmrAddress =
  "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG"

exampleDonationText :: Unicode
exampleDonationText =
  "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora."
