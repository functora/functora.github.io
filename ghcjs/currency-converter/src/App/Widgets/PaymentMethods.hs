module App.Widgets.PaymentMethods
  ( paymentMethodsViewer,
    paymentMethods,
  )
where

import App.Types
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import Functora.Prelude as Prelude
import qualified Material.Theme as Theme
import Miso hiding (at, view)

paymentMethodsViewer :: Model -> [PaymentMethod Unique] -> [View Action]
paymentMethodsViewer st = (>>= paymentMethodViewer st)

paymentMethodViewer :: Model -> PaymentMethod Unique -> [View Action]
paymentMethodViewer st mtd =
  Currency.moneyViewer
    ( Currency.defOpts
        & #optsLabel
        .~ Just "Total"
        & #optsShowZeroAmount
        .~ False
    )
    ( mtd ^. #paymentMethodMoney
    )
    <> FieldPairs.fieldPairsViewer st (mtd ^. #paymentMethodFieldPairs)

paymentMethods ::
  Model ->
  ATraversal' Model [PaymentMethod Unique] ->
  [View Action]
paymentMethods st optic = do
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
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodMoney
          . #moneyAmount
      )
      ( Field.defOpts
          & #optsPlaceholder
          .~ ("Total " <> idxTxt)
          & #optsDisabled
          .~ True
          & #optsLeadingWidget
          .~ Just
            ( Field.ModalWidget
                $ Field.ModalItemWidget
                  optic
                  idx
                  #paymentMethodFieldPairs
                  #paymentMethodModalState
            )
          & #optsTrailingWidget
          .~ Just
            ( Field.DeleteWidget optic idx [Theme.primary]
            )
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
      st
      ( cloneTraversal optic
          . ix idx
          . #paymentMethodFieldPairs
      )
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
