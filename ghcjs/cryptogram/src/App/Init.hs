module App.Init
  ( newModel,
  )
where

import qualified App.Jsm as Jsm
import App.Types
import Functora.Miso.Prelude

newModel ::
  MVar (Action -> IO ()) ->
  Maybe Model ->
  Maybe (St Unique) ->
  JSM Model
newModel sink mMod mApp = do
  defSt <- maybe (liftIO newSt) pure $ mMod ^? _Just . #modelState
  chatId <- Jsm.getChatId
  donate <- newDonateViewer
  pure
    Model
      { modelSink = sink,
        modelMenu = Closed,
        modelDonate = Closed,
        modelLoading = True,
        modelState = fromMaybe defSt mApp,
        modelChatId = chatId,
        modelDonateViewer = donate
      }

newDonateViewer :: (MonadIO m) => m [FieldPair DynamicField Unique]
newDonateViewer = do
  message <- newFieldPair "Hello, User!" $ DynamicFieldText exampleDonationText
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText exampleBtcAddress
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText exampleXmrAddress
  pure
    [ noTrunc message,
      qr btcMtd,
      qr xmrMtd
    ]
  where
    noTrunc :: FieldPair a b -> FieldPair a b
    noTrunc = #fieldPairValue . #fieldOpts . #fieldOptsTruncateLimit .~ Nothing
    qr :: FieldPair a b -> FieldPair a b
    qr =
      noTrunc
        . (#fieldPairValue . #fieldType .~ FieldTypeQrCode)
        . (#fieldPairValue . #fieldOpts . #fieldOptsAllowCopy .~ True)

exampleBtcAddress :: Unicode
exampleBtcAddress = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q"

exampleXmrAddress :: Unicode
exampleXmrAddress =
  "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG"

exampleDonationText :: Unicode
exampleDonationText =
  "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora."
