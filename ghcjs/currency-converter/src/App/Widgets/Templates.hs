module App.Widgets.Templates
  ( templates,
    unfilled,
    examples,
    newModel,
  )
where

import App.Prelude
import App.Types
import qualified App.Widgets.Cell as Cell
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Money hiding (Currency, Money)
import Functora.Rates
import qualified Functora.Web as Web
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import Miso hiding (URI, at, view)
import qualified Text.URI as URI

data Template = Template
  { templateName :: Text,
    templateIcon :: Text,
    templateDoc :: IO (StDoc Unique),
    templatePre :: IO (Maybe (Field DynamicField Unique)),
    templateIkm :: IO (Maybe (Field Text Unique))
  }
  deriving stock (Generic)

templates ::
  ALens' Model OpenedOrClosed ->
  [Template] ->
  Screen ->
  Model ->
  [View Action]
templates optic tpls sc st =
  if st ^. cloneLens optic == Closed
    then mempty
    else
      [ Dialog.dialog
          ( Dialog.config
              & Dialog.setOnClose closed
              & Dialog.setOpen True
          )
          ( Dialog.dialogContent
              Nothing
              [ Cell.grid mempty
                  $ ( do
                        tpl <- tpls
                        (icon, fun) <- [(tpl ^. #templateIcon, id)]
                        pure
                          . Cell.mediumCell
                          $ Button.raised
                            ( Button.config
                                & Button.setIcon (Just $ from @Text @String icon)
                                & Button.setOnClick (screen fun tpl)
                                & Button.setAttributes
                                  [ Theme.secondaryBg,
                                    class_ "fill"
                                  ]
                            )
                            ( from @Text @String $ tpl ^. #templateName
                            )
                    )
                  <> [ Cell.bigCell
                        $ Button.raised
                          ( Button.config
                              & Button.setOnClick goback
                              & Button.setIcon (Just "arrow_back")
                              & Button.setAttributes [class_ "fill"]
                          )
                          "Back"
                     ]
              ]
              mempty
          )
      ]
  where
    closed = pureUpdate 0 (& cloneLens optic .~ Closed)
    goback =
      pureUpdate 0
        $ (& #modelMenu .~ Opened)
        . (& cloneLens optic .~ Closed)
    screen fun tpl =
      PushUpdate $ do
        doc <- liftIO $ tpl ^. #templateDoc
        mPre <- liftIO $ tpl ^. #templatePre
        mIkm <- liftIO $ tpl ^. #templateIkm
        noPre <- newDynamicField $ DynamicFieldText mempty
        noIkm <- newPasswordField mempty
        let next =
              st
                & cloneLens optic
                .~ Closed
                & #modelState
                . #stScreen
                .~ fun sc
                & #modelState
                . #stDoc
                .~ doc
                & #modelState
                . #stPre
                .~ fromMaybe noPre mPre
                & #modelState
                . #stIkm
                .~ fromMaybe noIkm mIkm
                & #modelState
                . #stExt
                .~ Nothing
        uri <- URI.mkURI $ shareLink (fun sc) next
        new <- newModel (st ^. #modelWebOpts) (Just next) uri
        pure . ChanItem 0 $ const new

--
-- Templates
--

unfilled :: [Template]
unfilled =
  [ Template "Empty" "circle" emptyTemplate nil nil,
    Template "Text" "font_download" plainTemplate nil nil,
    Template "Donate" "volunteer_activism" donateTemplate nil nil,
    Template "Portfolio" "work" portfolioTemplate nil nil,
    Template "Secret" "lock" secretTemplate nil nil,
    Template "Invoice" "request_quote" invoiceTemplate nil nil
  ]
  where
    nil :: IO (Maybe (Field a b))
    nil = pure Nothing

emptyTemplate :: IO (StDoc Unique)
emptyTemplate = do
  conv <- newStConv
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = mempty,
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

plainTemplate :: IO (StDoc Unique)
plainTemplate = do
  conv <- newStConv
  msg <- newFieldPair mempty $ DynamicFieldText mempty
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [msg],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

donateTemplate :: IO (StDoc Unique)
donateTemplate = do
  conv <- newStConv
  msg <- newFieldPair mempty $ DynamicFieldText mempty
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText mempty
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText mempty
  fhead <- newDynamicTitleField "Hello, User!"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [msg, qr btcMtd, qr xmrMtd],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }
  where
    qr :: FieldPair a b -> FieldPair a b
    qr = (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)

portfolioTemplate :: IO (StDoc Unique)
portfolioTemplate = do
  conv <- newStConv
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField "Assets"
  phead <- newDynamicTitleField "Net worth"
  a0 <- newAsset "Cash" 0 usd
  a1 <- newAsset "Mobile wallet" 0 btc
  a2 <- newAsset "Private wallet" 0 xmr
  mtdUsd <- newPaymentMethod usd Nothing
  mtdBtc <- newPaymentMethod btc Nothing
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = mempty,
        stDocAssets = [a0, a1, a2],
        stDocPaymentMethods = [mtdUsd, mtdBtc],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = PaymentsBeforeAssets
      }

secretTemplate :: IO (StDoc Unique)
secretTemplate = do
  conv <- newStConv
  msg <- newFieldPair mempty $ DynamicFieldText mempty
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  stuff <- newAsset "Product" 0 usd
  delivery <- newAsset "Delivery" 0 usd
  method <- newPaymentMethod xmr $ Just mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [msg],
        stDocAssets = [stuff, delivery],
        stDocPaymentMethods = [method],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

invoiceTemplate :: IO (StDoc Unique)
invoiceTemplate = do
  conv <- newStConv
  fhead <- newDynamicTitleField "Invoice"
  ahead <- newDynamicTitleField "Purchase items"
  phead <- newDynamicTitleField "Payment methods"
  issuer <- newFieldPair "Issuer" $ DynamicFieldText mempty
  client <- newFieldPair "Client" $ DynamicFieldText mempty
  asset <- newProduct usd
  mtdUsd <- newPaymentMethod usd Nothing
  mtdBtc <- newPaymentMethod btc $ Just mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [issuer, client],
        stDocAssets = [asset],
        stDocPaymentMethods = [mtdUsd, mtdBtc],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }
  where
    newProduct cur = do
      lbl <- newTextField "Price"
      dsc <- newFieldPair "Product" $ DynamicFieldText mempty
      qty <- newFieldPair "Quantity" $ DynamicFieldNumber 1
      Asset
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure [dsc, qty]
        <*> pure Closed

--
-- Examples
--

examples :: [Template]
examples =
  [ Template "Empty" "circle" emptyTemplate nil nil,
    Template "Text" "font_download" plainExample nil nil,
    Template "Donate" "volunteer_activism" donateExample nil nil,
    Template "Portfolio" "work" portfolioExample nil nil,
    Template "Secret" "lock" secretExample pre ikm,
    Template "Invoice" "request_quote" invoiceExample nil nil
  ]
  where
    nil :: IO (Maybe (Field a b))
    nil = pure Nothing
    pre :: IO (Maybe (Field DynamicField Unique))
    pre = fmap Just . newDynamicField $ DynamicFieldText exampleSecretPre
    ikm :: IO (Maybe (Field Text Unique))
    ikm = fmap Just $ newPasswordField exampleSecretIkm

plainExample :: IO (StDoc Unique)
plainExample = do
  conv <- newStConv
  msg <- newFieldPair mempty $ DynamicFieldText examplePlainText
  fhead <- newDynamicTitleField "6102"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [msg],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

donateExample :: IO (StDoc Unique)
donateExample = do
  conv <- newStConv
  msg <- newFieldPair mempty $ DynamicFieldText exampleDonationText
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText exampleBtcAddress
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText exampleXmrAddress
  fhead <- newDynamicTitleField "Hello, User!"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [msg, qr btcMtd, qr xmrMtd],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }
  where
    qr :: FieldPair a b -> FieldPair a b
    qr = (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)

portfolioExample :: IO (StDoc Unique)
portfolioExample = do
  conv <- newStConv
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField "Assets"
  phead <- newDynamicTitleField "Net worth"
  a0 <- newAsset "Cash" 4000 usd
  a1 <- newAsset "US bank" 4500 usd
  a2 <- newAsset "EU bank" 2400 eur
  a3 <- newAsset "Mobile wallet" 0.042 btc
  a4 <- newAsset "Private wallet" 13.2 xmr
  mtdUsd <- newPaymentMethod usd Nothing
  mtdBtc <- newPaymentMethod btc Nothing
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = mempty,
        stDocAssets = [a0, a1, a2, a3, a4],
        stDocPaymentMethods = [mtdUsd, mtdBtc],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = PaymentsBeforeAssets
      }

secretExample :: IO (StDoc Unique)
secretExample = do
  conv <- newStConv
  msg <- newFieldPair mempty $ DynamicFieldText exampleSecretText
  fhead <- newDynamicTitleField "Dear Tommy,"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  stuff <- newAsset "Stuff" 100 usd
  delivery <- newAsset "Delivery" 25 usd
  method <- newPaymentMethod xmr $ Just exampleXmrAddress
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [msg],
        stDocAssets = [stuff, delivery],
        stDocPaymentMethods = [method],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

invoiceExample :: IO (StDoc Unique)
invoiceExample = do
  conv <- newStConv
  fhead <- newDynamicTitleField "Invoice #6102"
  ahead <- newDynamicTitleField "Purchase items"
  phead <- newDynamicTitleField "Payment methods"
  issuer <- newFieldPair "Issuer" $ DynamicFieldText "Alice's Grocery Store"
  client <- newFieldPair "Client" $ DynamicFieldText "Bob"
  tomato <- newProduct "Tomato" 2 4 usd
  beef <- newProduct "Beef" 0.5 10 eur
  mtdUsd <- newPaymentMethod usd Nothing
  mtdEur <- newPaymentMethod eur Nothing
  mtdBtc <- newPaymentMethod btc $ Just exampleBtcAddress
  mtdXmr <- newPaymentMethod xmr $ Just exampleXmrAddress
  pure
    StDoc
      { stDocConv = conv,
        stDocFieldPairs = [issuer, client],
        stDocAssets = [tomato, beef],
        stDocPaymentMethods = [mtdUsd, mtdEur, mtdBtc, mtdXmr],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }
  where
    newProduct name weight price cur = do
      lbl <- newTextField "Price (per kg)"
      dsc <- newFieldPair "Product" $ DynamicFieldText name
      qty <- newFieldPair "Weight (kg)" $ DynamicFieldNumber weight
      Asset
        <$> newMoney price cur
        <*> pure lbl
        <*> pure [dsc, qty]
        <*> pure Closed

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
      (liftIO emptyTemplate)
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
                  defExt
                )
      )
      mApp
  pure
    Model
      { modelMenu = Closed,
        modelTemplates = Closed,
        modelExamples = Closed,
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

exampleBtcAddress :: Text
exampleBtcAddress = "bc1qa3qk8d4mxl6qkpvahl5xvg6c5k33kmuwvt9v8q"

exampleXmrAddress :: Text
exampleXmrAddress =
  "48sTw2TvjuWKkaomi9J7gLExRUJLJCvUHLrbf8M8qmayQ9zkho1GYdCXVtpTPawNWH7mNS49N4E6HNDF95dtggMMCigrVyG"

examplePlainText :: Text
examplePlainText =
  "Executive Order 6102 required all persons to deliver on or before May 1, 1933, all but a small amount of gold coin, gold bullion, and gold certificates owned by them to the Federal Reserve in exchange for $20.67 (equivalent to $487 in 2023) per troy ounce. Under the Trading with the Enemy Act of 1917, as amended by the recently passed Emergency Banking Act of March 9, 1933, a violation of the order was punishable by fine up to $10,000 (equivalent to $235,000 in 2023), up to ten years in prison, or both."

exampleSecretText :: Text
exampleSecretText =
  "Bobby messed it up again, dude! I'll send you the dead drop location in the next secret message after he sends the money. You know the password. Cheers!"

exampleSecretPre :: Text
exampleSecretPre =
  "The password is \""
    <> exampleSecretIkm
    <> "\". THIS IS JUST AN EXAMPLE! NEVER SHARE REAL PASSWORDS THROUGH THE MESSAGE PREVIEW!"

exampleSecretIkm :: Text
exampleSecretIkm = "order6102"

exampleDonationText :: Text
exampleDonationText =
  "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora."

usd :: CurrencyInfo
usd = CurrencyInfo (CurrencyCode "usd") mempty

eur :: CurrencyInfo
eur = CurrencyInfo (CurrencyCode "eur") mempty

btc :: CurrencyInfo
btc = CurrencyInfo (CurrencyCode "btc") mempty

xmr :: CurrencyInfo
xmr = CurrencyInfo (CurrencyCode "xmr") mempty
