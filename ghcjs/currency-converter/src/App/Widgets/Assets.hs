module App.Widgets.Assets
  ( assetsViewer,
    assets,
  )
where

import App.Types
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import Functora.Prelude
import qualified Material.Theme as Theme
import Miso hiding (at, view)

assetsViewer :: Model -> [Asset Unique] -> [View Action]
assetsViewer st = (>>= assetViewer st)

assetViewer :: Model -> Asset Unique -> [View Action]
assetViewer st asset =
  FieldPairs.fieldPairsViewer st (asset ^. #assetFieldPairs)
    <> Currency.moneyViewer Currency.defOpts (asset ^. #assetPrice)

assets :: Model -> ATraversal' Model [Asset Unique] -> [View Action]
assets st optic = do
  idx <- fst <$> zip [0 ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
  assetWidget st optic idx

assetWidget ::
  Model ->
  ATraversal' Model [Asset Unique] ->
  Int ->
  [View Action]
assetWidget st optic idx =
  [ Field.ratioField
      st
      ( cloneTraversal optic
          . ix idx
          . #assetPrice
          . #moneyAmount
      )
      ( Field.defOpts
          & #optsPlaceholder
          .~ ("Price " <> idxTxt)
          & #optsLeadingWidget
          .~ Just
            ( Field.ModalWidget
                $ Field.ModalItemWidget
                  optic
                  idx
                  #assetFieldPairs
                  #assetModalState
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
          . #assetPrice
          . #moneyCurrency
      )
  ]
    <> FieldPairs.fieldPairs
      st
      ( cloneTraversal optic
          . ix idx
          . #assetFieldPairs
      )
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
