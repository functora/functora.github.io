module App.Widgets.PaymentMethods
  ( paymentMethods,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import qualified App.Widgets.Header as Header
import Functora.Prelude as Prelude
import Miso hiding (at, view)

paymentMethods ::
  Model ->
  ATraversal' Model [PaymentMethod Unique] ->
  [View Action]
paymentMethods st optic =
  (Header.header "Payments" (Just (Misc.newPaymentMethodAction st optic)) :) $ do
    idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
    paymentMethodWidget st optic idx

paymentMethodWidget ::
  Model ->
  ATraversal' Model [PaymentMethod Unique] ->
  Int ->
  [View Action]
paymentMethodWidget st optic idx =
  [ Field.ratioField
      st
      ( Right
          ( optic,
            idx,
            #paymentMethodMoney . #moneyAmount
          )
      )
      ( Field.defOpts
          & #optsDisabled
          .~ True
          & #optsPlaceholder
          .~ ("Total " <> idxTxt)
      ),
    Currency.selectCurrency
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodMoney
          . #moneyCurrency
      )
  ]
    <> FieldPairs.fieldPairs
      Footer
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodFieldPairs
      )
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)