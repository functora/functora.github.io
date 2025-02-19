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
  MVar (Action -> IO ()) ->
  Maybe Model ->
  Maybe (St Unique) ->
  m Model
newModel webOpts sink mMod mApp = do
  defSt <- maybe (liftIO newSt) pure $ mMod ^? _Just . #modelState
  donate <- newDonateViewer
  market <- maybe Rates.newMarket pure $ mMod ^? _Just . #modelMarket
  ct <- getCurrentTime
  pure
    Model
      { modelSink = sink,
        modelMenu = Closed,
        modelDonate = Closed,
        modelAppLinks = Closed,
        modelShareApp = Closed,
        modelPlaceOrder = Closed,
        modelRemoveOrder = Closed,
        modelMarketLinks = Closed,
        modelLoading = True,
        modelState = fromMaybe defSt mApp,
        modelUriViewer = mempty,
        modelDonateViewer = donate,
        modelCurrencies =
          fromMaybe [btc, usd, rub, cny] (mMod ^? _Just . #modelCurrencies),
        modelWebOpts = webOpts,
        modelMarket = market,
        modelTime = ct
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
