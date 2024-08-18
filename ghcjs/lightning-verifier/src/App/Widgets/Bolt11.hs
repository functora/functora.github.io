module App.Widgets.Bolt11
  ( bolt11,
  )
where

import App.Types
import qualified Functora.Bolt11 as Bolt11
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Prelude

bolt11 :: Model -> [View Action]
bolt11 st =
  if ln == mempty
    then mempty
    else case Bolt11.decodeBolt11 ln of
      Left e ->
        widget
          [ newFieldPairId mempty
              . DynamicFieldText
              $ from @Prelude.String @MisoString e
          ]
      Right b11 ->
        widget
          $ [ simple "Network" . Bolt11.bolt11Currency $ Bolt11.bolt11HRP b11,
              simple "Amount" . Bolt11.bolt11Amount $ Bolt11.bolt11HRP b11,
              simple "Timestamp" $ Bolt11.bolt11Timestamp b11
            ]
          <> fmap (simple "Tag") (Bolt11.bolt11Tags b11)
          <> [simple "Signature" $ Bolt11.bolt11Signature b11]
  where
    -- r = st ^. #modelState . #stDoc . #stDocLnPreimage . #fieldOutput
    ln = st ^. #modelState . #stDoc . #stDocLnInvoice . #fieldOutput
    simple x = newFieldPairId x . DynamicFieldText . inspect
    widget xs =
      FieldPairs.fieldPairsViewer
        FieldPairs.Args
          { FieldPairs.argsModel = xs,
            FieldPairs.argsOptic = id,
            FieldPairs.argsAction =
              \fun -> PushUpdate . Instant $ \next -> do
                void $ fun xs
                pure next
          }
