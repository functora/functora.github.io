module Functora.Miso.Widgets.Assets
  ( Args (..),
    Opts (..),
    defOpts,
    assetsViewer,
    assetsEditor,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Currency as Currency
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Money as Money
import Functora.Money hiding (Currency, Money, Text)
import qualified Material.Theme as Theme

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model [Asset Unique],
    argsAction :: (model -> JSM model) -> action,
    argsEmitter :: (model -> JSM model) -> JSM (),
    argsCurrencies :: Getter' model (NonEmpty CurrencyInfo)
  }
  deriving stock (Generic)

newtype Opts model = Opts
  { optsExtraOnClick :: model -> model
  }
  deriving stock (Generic)

defOpts :: Opts model
defOpts =
  Opts
    { optsExtraOnClick = id
    }

assetsViewer :: Args model action -> [View action]
assetsViewer args@Args {argsModel = st, argsOptic = optic} = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  assetViewer args idx

assetViewer :: Args model action -> Int -> [View action]
assetViewer
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter
    }
  idx =
    FieldPairs.fieldPairsViewer
      FieldPairs.Args
        { FieldPairs.argsModel = st,
          FieldPairs.argsOptic =
            cloneTraversal optic
              . ix idx
              . #assetFieldPairs,
          FieldPairs.argsAction = action,
          FieldPairs.argsEmitter = emitter
        }
      <> Money.moneyViewer
        Money.Args
          { Money.argsModel = st,
            Money.argsOptic =
              cloneTraversal optic
                . ix idx
                . #assetPrice,
            Money.argsAction = action
          }
        ( Money.defOpts
            & ( maybe id (#optsLabel .~)
                  $ st
                  ^? cloneTraversal optic
                  . ix idx
                  . #assetPriceLabel
                  . #fieldOutput
              )
        )

assetsEditor :: Args model action -> Opts model -> [View action]
assetsEditor args@Args {argsModel = st, argsOptic = optic} opts = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  assetEditor args opts idx

assetEditor ::
  forall model action.
  Args model action ->
  Opts model ->
  Int ->
  [View action]
assetEditor
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action,
      argsEmitter = emitter,
      argsCurrencies = currencies
    }
  Opts
    { optsExtraOnClick = extraOnClick
    }
  idx =
    [ Field.ratioField
        Field.Args
          { Field.argsModel = st,
            Field.argsOptic =
              cloneTraversal optic
                . ix idx
                . #assetPrice
                . #moneyAmount,
            Field.argsAction = action,
            Field.argsEmitter = emitter
          }
        ( Field.defOpts
            & #optsPlaceholder
            .~ ( if label == mempty
                  then idxTxt
                  else label <> " " <> idxTxt
               )
            & ( #optsLeadingWidget ::
                  Lens'
                    (Field.Opts model action)
                    (Maybe (Field.OptsWidgetPair model action))
              )
            .~ Just
              ( let w =
                      Field.ModalWidget
                        $ Field.ModalItemWidget
                          optic
                          idx
                          #assetFieldPairs
                          #assetPriceLabel
                          #assetModalState
                 in Field.OptsWidgetPair w w
              )
            & ( #optsTrailingWidget ::
                  Lens'
                    (Field.Opts model action)
                    (Maybe (Field.OptsWidgetPair model action))
              )
            .~ Just
              ( let w = Field.DeleteWidget optic idx [Theme.primary]
                 in Field.OptsWidgetPair w w
              )
        ),
      Currency.selectCurrency
        Currency.Args
          { Currency.argsModel = st,
            Currency.argsOptic =
              cloneTraversal optic
                . ix idx
                . #assetPrice
                . #moneyCurrency,
            Currency.argsAction = action,
            Currency.argsEmitter = emitter,
            Currency.argsCurrencies = currencies
          }
        Currency.defOpts
          { Currency.optsExtraOnClick = extraOnClick
          }
    ]
      <> FieldPairs.fieldPairsEditor
        FieldPairs.Args
          { FieldPairs.argsModel = st,
            FieldPairs.argsOptic =
              cloneTraversal optic
                . ix idx
                . #assetFieldPairs,
            FieldPairs.argsAction = action,
            FieldPairs.argsEmitter = emitter
          }
        FieldPairs.defOpts
    where
      idxTxt :: Unicode
      idxTxt = "#" <> inspect (idx + 1)
      label :: Unicode
      label =
        fromMaybe
          mempty
          ( st
              ^? cloneTraversal optic
              . ix idx
              . #assetPriceLabel
              . #fieldOutput
          )
