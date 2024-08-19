module App.Widgets.Bolt11
  ( bolt11,
  )
where

import App.Types
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
      (\x -> if x == mempty then mempty else preimageWidget x)
      r
  where
    rawLn = st ^. #modelState . #stDoc . #stDocLnInvoice . #fieldOutput
    rawR = st ^. #modelState . #stDoc . #stDocLnPreimage . #fieldOutput
    ln = first inspect $ B11.decodeBolt11 rawLn
    rh = ln >>= parsePreimageHash
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
        then success "Invoice and preimage match!"
        else failure "Invoice and preimage mismatch!"

invoiceWidget :: B11.Bolt11 -> [View Action]
invoiceWidget ln =
  Header.headerViewer "Invoice Details"
    <> pairs
      ( [ simple "Network" . B11.bolt11Currency $ B11.bolt11HRP ln,
          simple "Amount" . B11.bolt11Amount $ B11.bolt11HRP ln,
          simple "Timestamp" $ B11.bolt11Timestamp ln
        ]
          <> fmap (simple "Tag") (B11.bolt11Tags ln)
          <> [ simple "Signature" $ B11.bolt11Signature ln
             ]
      )
  where
    simple :: (Show a) => MisoString -> a -> FieldPair DynamicField Identity
    simple x =
      newFieldPairId x
        . DynamicFieldText
        . from @Prelude.String @MisoString
        . Prelude.show

preimageWidget :: ByteString -> [View Action]
preimageWidget r =
  Header.headerViewer "Preimage Details"
    <> pairs
      [ simple "Preimage" r,
        simple "Preimage Hash" $ sha256Hash r
      ]
  where
    simple :: MisoString -> ByteString -> FieldPair DynamicField Identity
    simple x =
      newFieldPairId x
        . DynamicFieldText
        . inspect

parsePreimage :: MisoString -> Either MisoString ByteString
parsePreimage rawR =
  case B16.decode . T.encodeUtf8 $ from @MisoString @Prelude.Text rawR of
    (r, "") -> Right r
    res -> Left $ "Bad preimage " <> inspect res

parsePreimageHash :: B11.Bolt11 -> Either MisoString ByteString
parsePreimageHash ln =
  case find B11.isPaymentHash $ B11.bolt11Tags ln of
    Just (B11.PaymentHash (B11.Hex rh)) -> Right rh
    _ -> Left "Bad invoice without preimage hash!"

pairs :: [FieldPair DynamicField f] -> [View Action]
pairs xs =
  FieldPairs.fieldPairsViewer
    FieldPairs.Args
      { FieldPairs.argsModel = xs,
        FieldPairs.argsOptic = id,
        FieldPairs.argsAction =
          \fun -> PushUpdate . Instant $ \next -> do
            void $ fun xs
            pure next
      }

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
