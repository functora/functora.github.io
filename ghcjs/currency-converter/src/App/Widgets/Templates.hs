module App.Widgets.Templates
  ( templates,
    unfilled,
    examples,
    newModel,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified Data.List.NonEmpty as NonEmpty
import qualified Functora.Aes as Aes
import Functora.Cfg
import Functora.Money hiding (Currency, Money)
import qualified Functora.Money as Money
import Functora.Prelude hiding (Field)
import Functora.Rates
import Functora.Rates (Market)
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Snackbar as Snackbar
import qualified Material.Theme as Theme
import Miso hiding (URI, at, view)
import qualified Text.URI as URI

data Template = Template
  { templateName :: Text,
    templateDoc :: MVar Market -> IO (StDoc Unique),
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
  [ Dialog.dialog
      ( Dialog.config
          & Dialog.setOnClose closed
          & Dialog.setOpen (Opened == st ^. cloneLens optic)
      )
      ( Dialog.dialogContent
          Nothing
          [ Cell.grid mempty
              $ ( do
                    tpl <- tpls
                    pure
                      . Cell.mediumCell
                      $ Button.raised
                        ( Button.config
                            & Button.setOnClick (screen tpl)
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
    screen tpl =
      PushUpdate $ do
        doc <- liftIO $ tpl ^. #templateDoc $ st ^. #modelMarket
        mPre <- liftIO $ tpl ^. #templatePre
        mIkm <- liftIO $ tpl ^. #templateIkm
        let next =
              st
                & cloneLens optic
                .~ Closed
                & #modelState
                . #stScreen
                .~ sc
                & #modelState
                . #stDoc
                .~ doc
                & #modelState
                . #stPre
                %~ flip fromMaybe mPre
                & #modelState
                . #stIkm
                %~ flip fromMaybe mIkm
                & #modelState
                . #stExt
                .~ Nothing
        uri <- URI.mkURI $ shareLink sc next
        new <- newModel (Just $ st ^. #modelMarket) uri
        pure . ChanItem 0 $ const new

--
-- Templates
--

unfilled :: [Template]
unfilled =
  [ Template "Empty" (const emptyTemplate) nil nil,
    Template "Text" (const plainTemplate) nil nil,
    Template "Donate" (const donateTemplate) nil nil,
    Template "Portfolio" portfolioTemplate nil nil,
    Template "Secret" secretExample pre ikm,
    Template "Invoice" invoiceTemplate nil nil
  ]
  where
    nil :: IO (Maybe (Field a b))
    nil = pure Nothing
    pre :: IO (Maybe (Field DynamicField Unique))
    pre = fmap Just . newDynamicField $ DynamicFieldText exampleSecretPre
    ikm :: IO (Maybe (Field Text Unique))
    ikm = fmap Just $ newPasswordField exampleSecretIkm

emptyTemplate :: IO (StDoc Unique)
emptyTemplate = do
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = mempty,
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

plainTemplate :: IO (StDoc Unique)
plainTemplate = do
  msg <- newFieldPair mempty $ DynamicFieldText mempty
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = [msg],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

donateTemplate :: IO (StDoc Unique)
donateTemplate = do
  msg <- newFieldPair mempty $ DynamicFieldText mempty
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText mempty
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText mempty
  fhead <- newDynamicTitleField "Hello, User!"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = [msg, qr btcMtd, qr xmrMtd],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = PaymentsBeforeAssets
      }
  where
    qr :: FieldPair a b -> FieldPair a b
    qr = (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)

portfolioTemplate :: MVar Market -> IO (StDoc Unique)
portfolioTemplate mkt = do
  usd <- newCurrencyInfo mkt $ CurrencyCode "usd"
  btc <- newCurrencyInfo mkt $ CurrencyCode "btc"
  xmr <- newCurrencyInfo mkt $ CurrencyCode "xmr"
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
      { stDocFieldPairs = mempty,
        stDocAssets = [a0, a1, a2],
        stDocPaymentMethods = [mtdUsd, mtdBtc],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = PaymentsBeforeAssets
      }

invoiceTemplate :: MVar Market -> IO (StDoc Unique)
invoiceTemplate mkt = do
  usd <- newCurrencyInfo mkt $ CurrencyCode "usd"
  btc <- newCurrencyInfo mkt $ CurrencyCode "btc"
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
      { stDocFieldPairs = [issuer, client],
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
  [ Template "Empty" (const emptyTemplate) nil nil,
    Template "Text" (const plainExample) nil nil,
    Template "Donate" (const donateExample) nil nil,
    Template "Portfolio" portfolioExample nil nil,
    Template "Secret" secretExample pre ikm,
    Template "Invoice" invoiceExample nil nil
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
  msg <- newFieldPair mempty $ DynamicFieldText examplePlainText
  fhead <- newDynamicTitleField "6102"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = [msg],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

donateExample :: IO (StDoc Unique)
donateExample = do
  msg <- newFieldPair mempty $ DynamicFieldText exampleDonationText
  btcMtd <- newFieldPair "BTC - Bitcoin" $ DynamicFieldText exampleBtcAddress
  xmrMtd <- newFieldPair "XMR - Monero" $ DynamicFieldText exampleXmrAddress
  fhead <- newDynamicTitleField "Hello, User!"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  pure
    StDoc
      { stDocFieldPairs = [msg, qr btcMtd, qr xmrMtd],
        stDocAssets = mempty,
        stDocPaymentMethods = mempty,
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = PaymentsBeforeAssets
      }
  where
    qr :: FieldPair a b -> FieldPair a b
    qr = (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)

portfolioExample :: MVar Market -> IO (StDoc Unique)
portfolioExample mkt = do
  usd <- newCurrencyInfo mkt $ CurrencyCode "usd"
  eur <- newCurrencyInfo mkt $ CurrencyCode "eur"
  btc <- newCurrencyInfo mkt $ CurrencyCode "btc"
  xmr <- newCurrencyInfo mkt $ CurrencyCode "xmr"
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField "Assets"
  phead <- newDynamicTitleField "Net worth"
  a0 <- newAsset "Cash" 3000 usd
  a1 <- newAsset "US bank" 4500 usd
  a2 <- newAsset "EU bank" 2300 eur
  a3 <- newAsset "Mobile wallet" 0.042 btc
  a4 <- newAsset "Private wallet" 13.2 xmr
  mtdUsd <- newPaymentMethod usd Nothing
  mtdBtc <- newPaymentMethod btc Nothing
  pure
    StDoc
      { stDocFieldPairs = mempty,
        stDocAssets = [a0, a1, a2, a3, a4],
        stDocPaymentMethods = [mtdUsd, mtdBtc],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = PaymentsBeforeAssets
      }

secretExample :: MVar Market -> IO (StDoc Unique)
secretExample mkt = do
  usd <- newCurrencyInfo mkt $ CurrencyCode "usd"
  xmr <- newCurrencyInfo mkt $ CurrencyCode "xmr"
  msg <- newFieldPair mempty $ DynamicFieldText exampleSecretText
  fhead <- newDynamicTitleField "Dear Tommy,"
  ahead <- newDynamicTitleField mempty
  phead <- newDynamicTitleField mempty
  stuff <- newAsset "Stuff" 100 usd
  delivery <- newAsset "Delivery" 25 usd
  method <- newPaymentMethod xmr $ Just exampleXmrAddress
  pure
    StDoc
      { stDocFieldPairs = [msg],
        stDocAssets = [stuff, delivery],
        stDocPaymentMethods = [method],
        stDocFieldPairsHeader = fhead,
        stDocAssetsHeader = ahead,
        stDocPaymentMethodsHeader = phead,
        stDocAssetsAndPaymentsLayout = AssetsBeforePayments
      }

invoiceExample :: MVar Market -> IO (StDoc Unique)
invoiceExample mkt = do
  usd <- newCurrencyInfo mkt $ CurrencyCode "usd"
  eur <- newCurrencyInfo mkt $ CurrencyCode "eur"
  btc <- newCurrencyInfo mkt $ CurrencyCode "btc"
  xmr <- newCurrencyInfo mkt $ CurrencyCode "xmr"
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
      { stDocFieldPairs = [issuer, client],
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

--
-- Misc
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
  btc <- newCurrencyInfo market $ CurrencyCode "btc"
  usd <- newCurrencyInfo market $ CurrencyCode "usd"
  topMoney <- newMoney 0 btc
  bottomMoney <- newMoney 0 usd
  ikm <- newPasswordField mempty
  km <- Aes.randomKm 32
  mApp <- unShareUri uri
  defDoc <- liftIO $ invoiceTemplate market
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

--
-- Const
--

exampleBtcAddress :: Text
exampleBtcAddress = "EXAMPLE"

exampleXmrAddress :: Text
exampleXmrAddress = "EXAMPLE"

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
    <> "\". THIS IS JUST AN EXAMPLE! NEVER SHARE REAL PASSWORDS THROUGH THE MESSAGE PREVIEW FEATURE!"

exampleSecretIkm :: Text
exampleSecretIkm = "order6102"

exampleDonationText :: Text
exampleDonationText =
  "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora."
