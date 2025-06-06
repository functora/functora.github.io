module App.Widgets.PaymentMethods
  ( paymentMethodsViewer,
    paymentMethods,
  )
where

import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import qualified Material.Theme as Theme
import Miso hiding (at, view)

paymentMethodsViewer :: Model -> [PaymentMethod Unique] -> [View Action]
paymentMethodsViewer st = (>>= paymentMethodViewer st)

paymentMethodViewer :: Model -> PaymentMethod Unique -> [View Action]
paymentMethodViewer st mtd =
  Currency.moneyViewer
    st
    ( Currency.defOpts
        & #optsLabel
        .~ (mtd ^. #paymentMethodMoneyLabel . #fieldOutput)
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
          .~ ( if label == mempty
                then idxTxt
                else label <> " " <> idxTxt
             )
          & #optsDisabled
          .~ True
          & #optsLeadingWidget
          .~ Just
            ( Field.ModalWidget
                $ Field.ModalItemWidget
                  optic
                  idx
                  #paymentMethodFieldPairs
                  #paymentMethodMoneyLabel
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
    label :: Text
    label =
      fromMaybe
        mempty
        ( st
            ^? cloneTraversal optic
            . ix idx
            . #paymentMethodMoneyLabel
            . #fieldOutput
        )
