module App.Widgets.PaymentMethods
  ( paymentMethods,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Amount as Amount
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Header as Header
import qualified App.Widgets.TextProps as TextProps
import Functora.Prelude as Prelude
import qualified Material.LayoutGrid as LayoutGrid
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
  [ Amount.amountSelect
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodMoney
          . #moneyAmount
      )
      ( Amount.opts
          & #optsDisabled
          .~ True
          & #optsPlaceholder
          .~ ("Total " <> idxTxt)
      ),
    Currency.currencySelect
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodMoney
          . #moneyCurrency
      ),
    Header.navHeaderComplex st optic #paymentMethodTextProps idx [LayoutGrid.span12]
  ]
    <> TextProps.textProps
      Footer
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodTextProps
      )
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
