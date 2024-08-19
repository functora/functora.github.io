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
import qualified Functora.Prelude as Prelude
import qualified Prelude

bolt11 :: Model -> [View Action]
bolt11 st =
  parserWidget ln b11
    <> (if isRight b11 then parserWidget ln rh else mempty)
    <> parserWidget hexR r
    <> fromRight mempty (verifierWidget [ln, hexR] <$> rh <*> r)
    <> either (const mempty) invoiceWidget b11
  where
    ln = st ^. #modelState . #stDoc . #stDocLnInvoice . #fieldOutput
    b11 = first inspect $ B11.decodeBolt11 ln
    rh = b11 >>= getPreimageHash
    hexR = st ^. #modelState . #stDoc . #stDocLnPreimage . #fieldOutput
    r = getPreimage hexR

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
invoiceWidget b11 =
  pairs
    $ [ simple "Network" . B11.bolt11Currency $ B11.bolt11HRP b11,
        simple "Amount" . B11.bolt11Amount $ B11.bolt11HRP b11,
        simple "Timestamp" $ B11.bolt11Timestamp b11
      ]
    <> fmap (simple "Tag") (B11.bolt11Tags b11)
    <> [ simple "Signature" $ B11.bolt11Signature b11
       ]
  where
    simple x =
      newFieldPairId x
        . DynamicFieldText
        . from @Prelude.String @MisoString
        . Prelude.show

getPreimage :: MisoString -> Either MisoString ByteString
getPreimage r =
  case B16.decode . T.encodeUtf8 $ from @MisoString @Prelude.Text r of
    (x, "") -> Right x
    res -> Left $ "Bad preimage " <> inspect res

getPreimageHash :: B11.Bolt11 -> Either MisoString ByteString
getPreimageHash b11 =
  case find B11.isPaymentHash $ B11.bolt11Tags b11 of
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
  addCssClass "app-success"
    $ pairs [newFieldPairId mempty $ DynamicFieldText msg]

failure :: MisoString -> [View Action]
failure msg =
  addCssClass "app-failure"
    $ pairs [newFieldPairId mempty $ DynamicFieldText msg]

addCssClass :: MisoString -> [View action] -> [View action]
addCssClass css = fmap $ \case
  Node x0 x1 x2 x3 x4 -> Node x0 x1 x2 (class_ css : x3) x4
  html -> html
