module Functora.Miso.Widgets.Assets
  ( assetsViewer,
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
    argsAction :: JSM (model -> model) -> action,
    argsCurrencies :: Getter' model (NonEmpty CurrencyInfo)
  }
  deriving stock (Generic)

assetsViewer :: Args model action -> [View action]
assetsViewer args@Args {argsModel = st, argsOptic = optic} = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  assetViewer args idx

assetViewer :: Args model action -> Int -> [View action]
assetViewer
  Args
    { argsModel = st,
      argsOptic = optic,
      argsAction = action
    }
  idx =
    FieldPairs.fieldPairsViewer
      FieldPairs.Args
        { FieldPairs.argsModel = st,
          FieldPairs.argsOptic =
            cloneTraversal optic
              . ix idx
              . #assetFieldPairs,
          FieldPairs.argsAction = action
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

assetsEditor :: Args model action -> [View action]
assetsEditor args@Args {argsModel = st, argsOptic = optic} = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  assetEditor args idx

assetEditor :: Args model action -> Int -> [View action]
assetEditor
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
                . #assetPrice
                . #moneyAmount,
            Field.argsAction = action
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
                    (Maybe (Field.OptsWidget model action))
              )
            .~ Just
              ( Field.ModalWidget
                  $ Field.ModalItemWidget
                    optic
                    idx
                    #assetFieldPairs
                    #assetPriceLabel
                    #assetModalState
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
                . #assetPrice
                . #moneyCurrency,
            Currency.argsAction = action,
            Currency.argsCurrencies = currencies
          }
    ]
      <> FieldPairs.fieldPairsEditor
        FieldPairs.Args
          { FieldPairs.argsModel = st,
            FieldPairs.argsOptic =
              cloneTraversal optic
                . ix idx
                . #assetFieldPairs,
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
              . #assetPriceLabel
              . #fieldOutput
          )
