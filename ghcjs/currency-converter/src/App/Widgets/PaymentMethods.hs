module App.Widgets.PaymentMethods
  ( paymentMethods,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Amount as Amount
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.TextProps as TextProps
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import Miso hiding (at, view)

paymentMethods ::
  Model ->
  ATraversal' Model [PaymentMethod Unique] ->
  [View Action]
paymentMethods st optic =
  zip [0 :: Int ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
    >>= paymentMethodsWidget st optic
    . fst

paymentMethodsWidget ::
  Model ->
  ATraversal' Model [PaymentMethod Unique] ->
  Int ->
  [View Action]
paymentMethodsWidget st optic idx =
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
      )
  ]
    <> TextProps.textProps
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodTextProps
      )
    <> [ button ("Duplicate " <> idxTxt) $ Misc.duplicateAt st optic idx,
         button ("Remove " <> idxTxt) $ Misc.removeAt st optic idx
       ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)

button ::
  forall a action.
  ( From a String
  ) =>
  a ->
  action ->
  View action
button label action =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    [ Button.raised
        ( Button.setOnClick action
            . Button.setAttributes
              [ class_ "fill",
                Theme.secondaryBg
              ]
            $ Button.config
        )
        ( from @a @String label
        )
    ]
