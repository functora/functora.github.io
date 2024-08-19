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
  if ln == mempty
    then mempty
    else case B11.decodeBolt11 ln of
      Left e ->
        red
          $ pairs
            [ newFieldPairId mempty
                . DynamicFieldText
                $ from @Prelude.String @MisoString e
            ]
      Right b11 ->
        ( case B11.bolt11Tags b11 >>= verifier r of
            [] | r /= mempty -> failure "Bad invoice without preimage hash!"
            xs -> xs
        )
          <> ( pairs
                $ [ simple "Network" . B11.bolt11Currency $ B11.bolt11HRP b11,
                    simple "Amount" . B11.bolt11Amount $ B11.bolt11HRP b11,
                    simple "Timestamp" $ B11.bolt11Timestamp b11
                  ]
                <> fmap (simple "Tag") (B11.bolt11Tags b11)
                <> [ simple "Signature" $ B11.bolt11Signature b11
                   ]
             )
  where
    r = st ^. #modelState . #stDoc . #stDocLnPreimage . #fieldOutput
    ln = st ^. #modelState . #stDoc . #stDocLnInvoice . #fieldOutput
    simple x =
      newFieldPairId x
        . DynamicFieldText
        . from @Prelude.String @MisoString
        . Prelude.show

verifier :: MisoString -> B11.Tag -> [View Action]
verifier "" _ =
  mempty
verifier r (B11.PaymentHash (B11.Hex expected)) =
  case B16.decode . T.encodeUtf8 $ from @MisoString @Prelude.Text r of
    (rbs, "") -> do
      let provided = sha256Hash rbs
      if expected == provided
        then success "Invoice and preimage match!"
        else failure "Invoice and preimage mismatch!"
    res ->
      failure $ "Bad preimage " <> inspect res
verifier _ _ =
  mempty

success :: MisoString -> [View Action]
success msg =
  green $ pairs [newFieldPairId mempty $ DynamicFieldText msg]

failure :: MisoString -> [View Action]
failure msg =
  red $ pairs [newFieldPairId mempty $ DynamicFieldText msg]

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

red :: [View action] -> [View action]
red = fmap $ \case
  Node x0 x1 x2 x3 x4 ->
    Node x0 x1 x2 (class_ "app-failure" : x3) x4
  html ->
    html

green :: [View action] -> [View action]
green = fmap $ \case
  Node x0 x1 x2 x3 x4 ->
    Node x0 x1 x2 (class_ "app-success" : x3) x4
  html ->
    html
