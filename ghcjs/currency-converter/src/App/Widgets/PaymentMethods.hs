module App.Widgets.PaymentMethods
  ( paymentMethods,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Amount as Amount
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Header as Header
import qualified App.Widgets.TextProps as TextProps
import Functora.Prelude as Prelude
import qualified Material.Theme as Theme
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
  [ Header.subHeader ("Method " <> idxTxt),
    Header.navHeader $ Header.newNav st optic idx,
    Amount.amountSelect
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
      )
  ]
    <> TextProps.textProps
      Footer
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodTextProps
      )
    <> [ Button.mediumButton
          [Theme.secondaryBg]
          ("Duplicate method " <> idxTxt)
          $ Misc.duplicateAt st optic idx,
         Button.mediumButton
          [Theme.secondaryBg]
          ("Remove method " <> idxTxt)
          $ Misc.removeAt st optic idx
       ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
