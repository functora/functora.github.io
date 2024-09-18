module App.Widgets.Templates
  ( newModel,
  )
where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Rates as Rates
import qualified Functora.Web as Web

newModel ::
  ( MonadThrow m,
    MonadUnliftIO m
  ) =>
  Web.Opts ->
  Maybe Model ->
  URI ->
  m Model
newModel webOpts mSt uri = do
  prod <- liftIO newBroadcastTChanIO
  cons <- liftIO . atomically $ dupTChan prod
  defSt <- maybe (liftIO newSt) pure $ mSt ^? _Just . #modelState
  mApp <- unShareUri uri
  donate <- newDonateViewer
  market <- maybe Rates.newMarket pure $ mSt ^? _Just . #modelMarket
  pure
    Model
      { modelFav = Closed,
        modelMenu = Closed,
        modelLinks = Closed,
        modelLoading = True,
        modelState = fromMaybe defSt mApp,
        modelFavMap = mempty,
        modelUriViewer = mempty,
        modelDonateViewer = donate,
        modelProducerQueue = prod,
        modelConsumerQueue = cons,
        modelCurrencies =
          fromMaybe [btc, usd, rub, cny] (mSt ^? _Just . #modelCurrencies),
        modelWebOpts = webOpts,
        modelMarket = market
      }

newDonateViewer :: (MonadIO m) => m [FieldPair DynamicField Unique]
newDonateViewer = do
  title <- newFieldPair mempty $ DynamicFieldText "Hello, User!"
  message <- newFieldPair mempty $ DynamicFieldText exampleDonationText
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText exampleBtcAddress
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText exampleXmrAddress
  pure
    [ title & #fieldPairValue . #fieldType .~ FieldTypeTitle,
      message,
      qr btcMtd,
      qr xmrMtd
    ]
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
