module App.Widgets.Templates
  ( templates,
    unfilled,
    examples,
  )
where

import App.Types
import qualified App.Widgets.Cell as Cell
import qualified Data.Text as T
import Functora.Money hiding (Currency, Money)
import Functora.Prelude hiding (Field)
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.Theme as Theme
import Miso hiding (at, view)

data Template = Template
  { templateName :: Text,
    templateMaker :: IO (StDoc Unique)
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
        doc <- liftIO $ tpl ^. #templateMaker
        pre <- newDynamicTitleField mempty
        pure
          $ ChanItem 0
          $ (cloneLens optic .~ Closed)
          . (#modelState . #stScreen .~ sc)
          . (#modelState . #stDoc .~ doc)
          . (#modelState . #stPre .~ pre)
          . (#modelState . #stExt .~ Nothing)

--
-- Templates
--

unfilled :: [Template]
unfilled =
  [ Template "Empty" emptyTemplate,
    Template "Text" plainTemplate,
    Template "Donate" donateTemplate,
    Template "Portfolio" portfolioTemplate,
    Template "Invoice" invoiceTemplate
  ]

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
  fhead <- newDynamicTitleField "Dear User,"
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

portfolioTemplate :: IO (StDoc Unique)
portfolioTemplate = do
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField "Assets"
  phead <- newDynamicTitleField "Net worth"
  a0 <- mkAsset "Cash" 0 usd
  a1 <- mkAsset "Mobile wallet" 0 btc
  a2 <- mkAsset "Private wallet" 0 xmr
  mtdUsd <- newPayment usd
  mtdBtc <- newPayment btc
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
  where
    mkAsset label amt cur = do
      lbl <-
        newTextField
          $ label
          <> " ("
          <> T.toUpper (inspectCurrencyInfo cur)
          <> ")"
      Asset
        <$> newMoney amt cur
        <*> pure lbl
        <*> pure mempty
        <*> pure Closed
    newPayment cur = do
      lbl <- newTextField $ "Total " <> T.toUpper (inspectCurrencyInfo cur)
      PaymentMethod
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure mempty
        <*> pure Closed

invoiceTemplate :: IO (StDoc Unique)
invoiceTemplate = do
  fhead <- newDynamicTitleField "Invoice"
  ahead <- newDynamicTitleField "Purchase items"
  phead <- newDynamicTitleField "Payment methods"
  issuer <- newFieldPair "Issuer" $ DynamicFieldText mempty
  client <- newFieldPair "Client" $ DynamicFieldText mempty
  asset <- newProduct usd
  mtdUsd <- newFiatPayment usd
  mtdBtc <- newCryptoPayment btc mempty
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
    newFiatPayment cur = do
      lbl <- newTextField $ "Total " <> T.toUpper (inspectCurrencyInfo cur)
      PaymentMethod
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure mempty
        <*> pure Closed
    newCryptoPayment cur addr = do
      lbl <- newTextField $ "Total " <> T.toUpper (inspectCurrencyInfo cur)
      address <-
        fmap (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)
          . newFieldPair ("Address " <> T.toUpper (inspectCurrencyInfo cur))
          $ DynamicFieldText addr
      PaymentMethod
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure [address]
        <*> pure Closed

--
-- Examples
--

examples :: [Template]
examples =
  [ Template "Empty" emptyTemplate,
    Template "Text" plainExample,
    Template "Donate" donateExample,
    Template "Portfolio" portfolioExample,
    Template "Invoice" invoiceExample
  ]

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
  fhead <- newDynamicTitleField "Dear User,"
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

portfolioExample :: IO (StDoc Unique)
portfolioExample = do
  fhead <- newDynamicTitleField mempty
  ahead <- newDynamicTitleField "Assets"
  phead <- newDynamicTitleField "Net worth"
  a0 <- mkAsset "Cash" 3000 usd
  a1 <- mkAsset "US bank" 4500 usd
  a2 <- mkAsset "EU bank" 2300 eur
  a3 <- mkAsset "Mobile wallet" 0.042 btc
  a4 <- mkAsset "Private wallet" 13.2 xmr
  mtdUsd <- newPayment usd
  mtdBtc <- newPayment btc
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
  where
    mkAsset label amt cur = do
      lbl <-
        newTextField
          $ label
          <> " ("
          <> T.toUpper (inspectCurrencyInfo cur)
          <> ")"
      Asset
        <$> newMoney amt cur
        <*> pure lbl
        <*> pure mempty
        <*> pure Closed
    newPayment cur = do
      lbl <- newTextField $ "Total " <> T.toUpper (inspectCurrencyInfo cur)
      PaymentMethod
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure mempty
        <*> pure Closed

invoiceExample :: IO (StDoc Unique)
invoiceExample = do
  fhead <- newDynamicTitleField "Invoice #6102"
  ahead <- newDynamicTitleField "Purchase items"
  phead <- newDynamicTitleField "Payment methods"
  issuer <- newFieldPair "Issuer" $ DynamicFieldText "Alice's Grocery Store"
  client <- newFieldPair "Client" $ DynamicFieldText "Bob"
  tomato <- newProduct "Tomato" 2 4 usd
  beef <- newProduct "Beef" 0.5 10 eur
  mtdUsd <- newFiatPayment usd
  mtdEur <- newFiatPayment eur
  mtdBtc <- newCryptoPayment btc exampleBtcAddress
  mtdXmr <- newCryptoPayment xmr exampleXmrAddress
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
    newFiatPayment cur = do
      lbl <- newTextField $ "Total " <> T.toUpper (inspectCurrencyInfo cur)
      PaymentMethod
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure mempty
        <*> pure Closed
    newCryptoPayment cur addr = do
      lbl <- newTextField $ "Total " <> T.toUpper (inspectCurrencyInfo cur)
      address <-
        fmap (& #fieldPairValue . #fieldType .~ FieldTypeQrCode)
          . newFieldPair ("Address " <> T.toUpper (inspectCurrencyInfo cur))
          $ DynamicFieldText addr
      PaymentMethod
        <$> newMoney 0 cur
        <*> pure lbl
        <*> pure [address]
        <*> pure Closed

--
-- Misc
--

btc :: CurrencyInfo
btc =
  CurrencyInfo
    { currencyInfoCode = CurrencyCode "btc",
      currencyInfoText = mempty
    }

xmr :: CurrencyInfo
xmr =
  CurrencyInfo
    { currencyInfoCode = CurrencyCode "xmr",
      currencyInfoText = mempty
    }

usd :: CurrencyInfo
usd =
  CurrencyInfo
    { currencyInfoCode = CurrencyCode "usd",
      currencyInfoText = mempty
    }

eur :: CurrencyInfo
eur =
  CurrencyInfo
    { currencyInfoCode = CurrencyCode "eur",
      currencyInfoText = mempty
    }

exampleBtcAddress :: Text
exampleBtcAddress = "EXAMPLE"

exampleXmrAddress :: Text
exampleXmrAddress = "EXAMPLE"

examplePlainText :: Text
examplePlainText =
  "Executive Order 6102 required all persons to deliver on or before May 1, 1933, all but a small amount of gold coin, gold bullion, and gold certificates owned by them to the Federal Reserve in exchange for $20.67 (equivalent to $487 in 2023) per troy ounce. Under the Trading with the Enemy Act of 1917, as amended by the recently passed Emergency Banking Act of March 9, 1933, a violation of the order was punishable by fine up to $10,000 (equivalent to $235,000 in 2023), up to ten years in prison, or both."

exampleDonationText :: Text
exampleDonationText =
  "I'm Functora, the creator of this software. If you're enjoying it, a donation would be greatly appreciated. Sincerely yours, Functora."