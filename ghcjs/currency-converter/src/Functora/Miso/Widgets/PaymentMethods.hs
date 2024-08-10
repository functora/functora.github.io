module Functora.Miso.Widgets.PaymentMethods
  ( paymentMethodsViewer,
    paymentMethods,
  )
where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Money as Money
import Functora.Money hiding (Currency, Money, Text)
import qualified Material.Theme as Theme

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model [PaymentMethod Unique],
    argsAction :: JSM (model -> model) -> action,
    argsCurrencies :: Getter' model (NonEmpty CurrencyInfo)
  }
  deriving stock (Generic)

paymentMethodsViewer :: Args model action -> [View action]
paymentMethodsViewer args@Args {argsModel = st, argsOptic = optic} = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  paymentMethodViewer args idx

paymentMethodViewer :: Args model action -> Int -> [View action]
paymentMethodViewer
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action
    }
  idx =
    Money.moneyViewer
      Money.Args
        { Money.argsModel = st,
          Money.argsOptic =
            cloneTraversal optic
              . ix idx
              . #paymentMethodMoney,
          Money.argsAction = action
        }
      ( Money.defOpts
          & ( maybe id (#optsLabel .~)
                $ st
                ^? cloneTraversal optic
                . ix idx
                . #paymentMethodMoneyLabel
                . #fieldOutput
            )
          & #optsShowZeroAmount
          .~ False
      )
      <> FieldPairs.fieldPairsViewer
        FieldPairs.Args
          { FieldPairs.argsModel = st,
            FieldPairs.argsOptic =
              cloneTraversal optic
                . ix idx
                . #paymentMethodFieldPairs,
            FieldPairs.argsAction = action
          }

paymentMethods :: Args model action -> [View action]
paymentMethods args@Args {argsModel = st, argsOptic = optic} = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  paymentMethodWidget args idx

paymentMethodWidget ::
  forall model action. Args model action -> Int -> [View action]
paymentMethodWidget
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsCurrencies = currencies
    }
  idx =
    [ Field.ratioField
        Field.Args
          { Field.argsModel = st,
            Field.argsOptic =
              cloneTraversal optic
                . ix idx
                . #paymentMethodMoney
                . #moneyAmount,
            Field.argsAction = action
          }
        ( Field.defOpts @model @action
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
            & ( #optsTrailingWidget ::
                  Lens'
                    (Field.Opts model action)
                    (Maybe (Field.OptsWidget model action))
              )
            .~ Just
              ( Field.DeleteWidget optic idx [Theme.primary]
              )
        ),
      Currency.selectCurrency
        Currency.Args
          { Currency.argsModel = st,
            Currency.argsOptic =
              cloneTraversal optic
                . ix idx
                . #paymentMethodMoney
                . #moneyCurrency,
            Currency.argsAction = action,
            Currency.argsCurrencies = currencies
          }
    ]
      <> FieldPairs.fieldPairs
        FieldPairs.Args
          { FieldPairs.argsModel = st,
            FieldPairs.argsOptic =
              cloneTraversal optic
                . ix idx
                . #paymentMethodFieldPairs,
            FieldPairs.argsAction = action
          }
    where
      idxTxt :: MisoString
      idxTxt = "#" <> inspect (idx + 1)
      label :: MisoString
      label =
        fromMaybe
          mempty
          ( st
              ^? cloneTraversal optic
              . ix idx
              . #paymentMethodMoneyLabel
              . #fieldOutput
          )
