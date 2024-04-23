module App.Widgets.Assets
  ( assets,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified App.Widgets.Amount as Amount
import qualified App.Widgets.Currency as Currency
import qualified App.Widgets.TextProps as TextProps
import Functora.Money
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String (ms)

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
  [ titleWidget $ "Item " <> idxTxt,
    Amount.amountSelect
      st
      ( cloneTraversal optic
          . ix idx
          . #assetMoney
          . #moneyCurrency
          . #currencyOutput
          . to (inspectCurrencyInfo @Text)
      )
      ( cloneTraversal optic
          . ix idx
          . #assetMoney
          . #moneyAmount
      )
      id,
    Currency.currencySelect
      st
      ( cloneTraversal optic
          . ix idx
          . #assetMoney
          . #moneyCurrency
      )
  ]
    <> TextProps.textProps
      st
      ( cloneTraversal optic
          . ix idx
          . #assetProps
      )
    <> [ button ("Duplicate item " <> idxTxt) $ Misc.duplicateAt st optic idx,
         button ("Remove item " <> idxTxt) $ Misc.removeAt st optic idx
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

titleWidget :: Text -> View Action
titleWidget txt =
  LayoutGrid.cell
    [ LayoutGrid.span12,
      Typography.body1,
      style_
        [ ("text-align", "center")
        ]
    ]
    [ Miso.text $ ms txt
    ]
