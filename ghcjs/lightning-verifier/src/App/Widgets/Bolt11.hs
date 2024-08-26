module App.Widgets.Bolt11
  ( bolt11,
  )
where

import App.Types
import qualified Bitcoin.Address as Btc
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as T
import qualified Functora.Bolt11 as B11
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Header as Header
import qualified Functora.Prelude as Prelude
import qualified Prelude

bolt11 :: Model -> [View Action]
bolt11 st =
  parserWidget rawLn ln
    <> (if isRight ln then parserWidget rawLn rh else mempty)
    <> parserWidget rawR r
    <> fromRight mempty (verifierWidget [rawLn, rawR] <$> rh <*> r)
    <> either (const mempty) invoiceWidget ln
    <> either
      (const mempty)
      (\bsR -> if bsR == mempty then mempty else preimageWidget rawR bsR)
      r
  where
    rawLn :: MisoString
    rawLn = st ^. #modelState . #stDoc . #stDocLnInvoice . #fieldOutput
    rawR :: MisoString
    rawR = st ^. #modelState . #stDoc . #stDocLnPreimage . #fieldOutput
    ln :: Either MisoString B11.Bolt11
    ln =
      first (mappend "Bad invoice - " . from @Prelude.String @MisoString)
        . B11.decodeBolt11
        $ from @MisoString @Prelude.Text rawLn
    rh :: Either MisoString ByteString
    rh = ln >>= parsePreimageHash
    r :: Either MisoString ByteString
    r = parsePreimage rawR

parserWidget :: MisoString -> Either MisoString a -> [View Action]
parserWidget src = \case
  Left e | src /= mempty -> failure e
  _ -> mempty

verifierWidget :: [MisoString] -> ByteString -> ByteString -> [View Action]
verifierWidget src rh r =
  if any (== mempty) src
    then mempty
    else
      if rh == sha256Hash r
        then success "The preimage matches the invoice"
        else failure "The preimage does not match the invoice"

invoiceWidget :: B11.Bolt11 -> [View Action]
invoiceWidget ln =
  Header.headerViewer "Invoice Details"
    <> pairs
      ( [ pair "Network"
            $ case B11.bolt11HrpNet $ B11.bolt11Hrp ln of
              B11.BitcoinMainnet -> "Bitcoin Mainnet"
              B11.BitcoinTestnet -> "Bitcoin Testnet"
              B11.BitcoinRegtest -> "Bitcoin Regtest"
              B11.BitcoinSignet -> "Bitcoin Signet",
          pair "Amount"
            . maybe "0" B11.inspectBolt11HrpAmt
            . B11.bolt11HrpAmt
            $ B11.bolt11Hrp ln,
          pair "Timestamp"
            . inspect
            $ B11.bolt11Timestamp ln
        ]
          <> ( B11.bolt11Tags ln
                >>= invoiceTagWidget
             )
          <> [ pair "Signature"
                . B11.inspectHex
                $ B11.bolt11SigVal sig,
               pair "Pubkey Recovery Flag"
                . inspect
                $ B11.bolt11SigRecoveryFlag sig
             ]
      )
  where
    sig = B11.bolt11Signature ln

invoiceTagWidget :: B11.Tag -> [FieldPair DynamicField Identity]
invoiceTagWidget = \case
  B11.PaymentHash x -> hex "Preimage Hash" x
  B11.PaymentSecret x -> hex "Payment Secret" x
  B11.Description x -> pure . pair "Description" $ inspect x
  B11.AdditionalMetadata x -> w5s "Additional Metadata" x
  B11.PayeePubkey x -> hex "Payee Pubkey" x
  B11.DescriptionHash x -> hex "Description Hash" x
  B11.Expiry x -> pure . pair "Expiry" $ inspect x
  B11.MinFinalCltvExpiry x -> pure . pair "Min Final CLTV Expiry" $ inspect x
  B11.OnchainFallback x -> do
    --
    -- TODO : do not ignore failure?
    --
    txt <- either (const mempty) pure . decodeUtf8' $ Btc.renderAddress x
    pure $ pair "Onchain Fallback" $ from @Prelude.Text @MisoString txt
  B11.ExtraRouteInfo -> mempty
  B11.Features x -> pure . pair "Feature Bits" $ inspect x
  B11.UnknownTag {} -> mempty
  B11.UnparsedTag {} -> mempty
  where
    w5s :: MisoString -> [B11.Word5] -> [FieldPair DynamicField Identity]
    w5s x =
      pure
        . pair x
        . inspect
        . fmap fromEnum
    hex :: MisoString -> B11.Hex -> [FieldPair DynamicField Identity]
    hex x =
      pure
        . pair x
        . B11.inspectHex

preimageWidget :: MisoString -> ByteString -> [View Action]
preimageWidget rawR r =
  Header.headerViewer "Preimage Details"
    <> pairs
      [ pair "Preimage" rawR,
        pair "Preimage Hash" . inspect @ByteString $ sha256Hash r
      ]

parsePreimage :: MisoString -> Either MisoString ByteString
parsePreimage rawR =
  case B16.decode . T.encodeUtf8 $ from @MisoString @Prelude.Text rawR of
    (r, "") -> Right r
    (_, e) ->
      Left
        $ "Bad preimage - non hex leftover "
        <> from @Prelude.String @MisoString (Prelude.show e)

parsePreimageHash :: B11.Bolt11 -> Either MisoString ByteString
parsePreimageHash ln =
  case find B11.isPaymentHash $ B11.bolt11Tags ln of
    Just (B11.PaymentHash (B11.Hex rh)) -> Right rh
    _ -> Left "Bad invoice - no preimage hash"

pair :: MisoString -> MisoString -> FieldPair DynamicField Identity
pair x =
  newFieldPairId x
    . DynamicFieldText

pairs :: [FieldPair DynamicField f] -> [View Action]
pairs raw =
  FieldPairs.fieldPairsViewer
    FieldPairs.Args
      { FieldPairs.argsModel = xs,
        FieldPairs.argsOptic = id,
        FieldPairs.argsAction =
          \fun -> PushUpdate . Instant $ \next -> do
            void $ fun xs
            pure next
      }
  where
    xs =
      filter
        ( \x ->
            inspectDynamicField (x ^. #fieldPairValue . #fieldOutput)
              /= mempty
        )
        raw

success :: MisoString -> [View Action]
success msg =
  css "app-success"
    $ pairs [newFieldPairId mempty $ DynamicFieldText msg]

failure :: MisoString -> [View Action]
failure msg =
  css "app-failure"
    $ pairs [newFieldPairId mempty $ DynamicFieldText msg]

css :: MisoString -> [View action] -> [View action]
css x = fmap $ \case
  Node x0 x1 x2 x3 x4 -> Node x0 x1 x2 (class_ x : x3) x4
  html -> html
