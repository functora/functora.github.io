module App.Widgets.Assets
  ( assets,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Amount as Amount
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.TextInput as TextInput
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import Miso hiding (at, view)

assets :: Model -> ATraversal' Model [Asset Unique] -> [View Action]
assets st optic =
  zip [0 :: Int ..] (fromMaybe mempty $ st ^? cloneTraversal optic)
    >>= assetsWidget st optic
    . fst

assetsWidget ::
  Model ->
  ATraversal' Model [Asset Unique] ->
  Int ->
  [View Action]
assetsWidget st optic idx =
  [ TextInput.textInput
      st
      ( "Description " <> idxTxt
      )
      ( cloneTraversal optic
          . ix idx
          . #assetDescription
      ),
    Amount.amountSelect
      st
      ( to . const $ "Quantity " <> idxTxt
      )
      ( cloneTraversal optic
          . ix idx
          . #assetQuantity
      )
      id,
    Amount.amountSelect
      st
      ( to . const $ "Price " <> idxTxt
      )
      ( cloneTraversal optic
          . ix idx
          . #assetPrice
          . #moneyAmount
      )
      id,
    Currency.currencySelect
      st
      ( cloneTraversal optic
          . ix idx
          . #assetPrice
          . #moneyCurrency
      )
  ]
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
