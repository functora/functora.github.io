module App.Widgets.Bolt11
  ( bolt11Viewer,
    makeBolt11Viewer,
    mergeBolt11Viewers,
  )
where

import App.Types
import qualified Bitcoin.Address as Btc
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Functora.Bolt11 as B11
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Header as Header
import qualified Functora.Prelude as Prelude
import qualified Prelude

bolt11Viewer :: Model -> [View Action]
bolt11Viewer st =
  success
    ( pairs st mempty
        $ #modelState
        . #stDoc
        . #stDocSuccessViewer
    )
    <> failure
      ( pairs st mempty
          $ #modelState
          . #stDoc
          . #stDocFailureViewer
      )
    <> pairs
      st
      ( Header.headerViewer "Invoice Details"
      )
      ( #modelState . #stDoc . #stDocLnInvoiceViewer
      )
    <> pairs
      st
      ( Header.headerViewer "Preimage Details"
      )
      ( #modelState . #stDoc . #stDocLnPreimageViewer
      )

pairs ::
  ( Foldable1 f
  ) =>
  Model ->
  [View Action] ->
  ATraversal' Model [FieldPair DynamicField f] ->
  [View Action]
pairs st header optic =
  if null widget
    then mempty
    else header <> widget
  where
    widget =
      FieldPairs.fieldPairsViewer
        FieldPairs.Args
          { FieldPairs.argsModel = st,
            FieldPairs.argsOptic = optic,
            FieldPairs.argsAction = PushUpdate . Instant
          }

pair :: MisoString -> MisoString -> FieldPair DynamicField Identity
pair x =
  newFieldPairId x
    . DynamicFieldText

success :: [View Action] -> [View Action]
success = css "app-success"

failure :: [View Action] -> [View Action]
failure = css "app-failure"

css :: MisoString -> [View action] -> [View action]
css x = fmap $ \case
  Node x0 x1 x2 x3 x4 -> Node x0 x1 x2 (class_ x : x3) x4
  html -> html

inspectTimestamp :: Int -> MisoString
inspectTimestamp =
  inspect
    . posixSecondsToUTCTime
    . Prelude.fromInteger
    . from @Int @Integer

makeBolt11Viewer :: StDoc Identity -> StDoc Identity
makeBolt11Viewer st =
  let lnFields =
        if rawLn == mempty
          then pure mempty
          else bimap plain invoiceFields ln
      preFields =
        if rawR == mempty
          then pure mempty
          else bimap plain (preimageFields rawR) r
      verifierFields =
        if any @[MisoString] (== mempty) [rawLn, rawR]
          then pure mempty
          else case verifyPreimage <$> rh <*> r of
            Left {} -> pure mempty
            Right x -> bimap plain plain x
   in st
        & #stDocSuccessViewer
        .~ fromRight mempty verifierFields
        & #stDocFailureViewer
        .~ ( fromLeft mempty lnFields
              <> fromLeft mempty preFields
              <> fromLeft mempty verifierFields
           )
        & #stDocLnInvoiceViewer
        .~ fromRight mempty lnFields
        & #stDocLnPreimageViewer
        .~ fromRight mempty preFields
  where
    rawLn :: MisoString
    rawLn = st ^. #stDocLnInvoice . #fieldOutput
    rawR :: MisoString
    rawR = st ^. #stDocLnPreimage . #fieldOutput
    ln :: Either MisoString B11.Bolt11
    ln =
      first (mappend "Bad invoice - " . from @Prelude.String @MisoString)
        . B11.decodeBolt11
        $ from @MisoString @Prelude.Text rawLn
    rh :: Either MisoString ByteString
    rh = ln >>= parsePreimageHash
    r :: Either MisoString ByteString
    r = parsePreimage rawR

mergeBolt11Viewers :: (Foldable1 f) => StDoc f -> StDoc f -> StDoc f
mergeBolt11Viewers next prev =
  prev
    & #stDocSuccessViewer
    %~ mergeFieldPairs (stDocSuccessViewer next)
    & #stDocFailureViewer
    %~ mergeFieldPairs (stDocFailureViewer next)
    & #stDocLnInvoiceViewer
    %~ mergeFieldPairs (stDocLnInvoiceViewer next)
    & #stDocLnPreimageViewer
    %~ mergeFieldPairs (stDocLnPreimageViewer next)

plain :: MisoString -> [FieldPair DynamicField Identity]
plain =
  (: mempty) . newFieldPairId mempty . DynamicFieldText

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

verifyPreimage ::
  ByteString ->
  ByteString ->
  Either MisoString MisoString
verifyPreimage rh r =
  if rh == sha256Hash r
    then Right "The preimage matches the invoice"
    else Left "The preimage does not match the invoice"

invoiceFields :: B11.Bolt11 -> [FieldPair DynamicField Identity]
invoiceFields ln =
  [ pair "Network"
      $ case B11.bolt11HrpNet $ B11.bolt11Hrp ln of
        B11.BitcoinMainnet -> "Bitcoin Mainnet"
        B11.BitcoinTestnet -> "Bitcoin Testnet"
        B11.BitcoinRegtest -> "Bitcoin Regtest"
        B11.BitcoinSignet -> "Bitcoin Signet",
    pair "Amount"
      . maybe "0" B11.inspectBolt11HrpAmt
      . B11.bolt11HrpAmt
      $ B11.bolt11Hrp ln,
    pair "Created At"
      . inspectTimestamp
      $ B11.bolt11Timestamp ln
  ]
    <> ( B11.bolt11Tags ln
          >>= invoiceFieldsTag ln
       )
    <> [ pair "Signature"
          . B11.inspectHex
          $ B11.bolt11SigVal sig,
         pair "Pubkey Recovery Flag"
          . inspect
          $ B11.bolt11SigRecoveryFlag sig
       ]
  where
    sig = B11.bolt11Signature ln

invoiceFieldsTag :: B11.Bolt11 -> B11.Tag -> [FieldPair DynamicField Identity]
invoiceFieldsTag ln = \case
  B11.PaymentHash x -> hex "Preimage Hash" x
  B11.PaymentSecret x -> hex "Payment Secret" x
  B11.Description x -> pure . pair "Description" $ inspect x
  B11.AdditionalMetadata x -> w5s "Additional Metadata" x
  B11.PayeePubkey x -> hex "Payee Pubkey" x
  B11.DescriptionHash x -> hex "Description Hash" x
  B11.Expiry x ->
    pure . pair "Expires At" . inspectTimestamp $ x + B11.bolt11Timestamp ln
  B11.MinFinalCltvExpiry x ->
    pure . pair "Min Final CLTV Expiry" $ inspect x <> " Blocks"
  B11.OnchainFallback x -> do
    --
    -- TODO : do not ignore failure?
    --
    txt <- either (const mempty) pure . decodeUtf8' $ Btc.renderAddress x
    pure $ pair "Onchain Fallback" $ from @Prelude.Text @MisoString txt
  B11.ExtraRouteInfo x ->
    pure
      . pair "Extra Routing Info"
      . either (const mempty) (from @Prelude.Text @MisoString)
      . decodeUtf8'
      . from @BL.ByteString @ByteString
      $ A.encode x
  B11.Features x -> pure . pair "Feature Bits" $ B11.inspectFeatures x
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

preimageFields :: MisoString -> ByteString -> [FieldPair DynamicField Identity]
preimageFields rawR r =
  [ pair "Preimage" rawR,
    pair "Preimage Hash" . inspect @ByteString $ sha256Hash r
  ]
