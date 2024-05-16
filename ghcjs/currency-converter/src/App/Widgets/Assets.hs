module App.Widgets.Assets
  ( assets,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.Field as Field
import qualified App.Widgets.FieldPairs as FieldPairs
import qualified App.Widgets.Header as Header
import Functora.Prelude
import Miso hiding (at, view)

assets :: Model -> ATraversal' Model [Asset Unique] -> [View Action]
assets st optic =
  (Header.header "Assets" (Just (Misc.newAssetAction st optic)) :) $ do
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
      ( Right
          ( optic,
            idx,
            #assetPrice . #moneyAmount
          )
      )
      ( Field.defOpts
          & #optsStaticType
          .~ True
          & #optsPlaceholder
          .~ ("Price " <> idxTxt)
      ),
    Currency.selectCurrency
      st
      ( cloneTraversal optic
          . ix idx
          . #assetPrice
          . #moneyCurrency
      ),
    Field.ratioField
      st
      ( Right
          ( optic,
            idx,
            #assetQuantity
          )
      )
      ( Field.defOpts
          & #optsStaticType
          .~ True
          & #optsPlaceholder
          .~ ("Quantity " <> idxTxt)
      )
  ]
    <> FieldPairs.fieldPairs
      Footer
      st
      ( cloneTraversal optic
          . ix idx
          . #assetFieldPairs
      )
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)
