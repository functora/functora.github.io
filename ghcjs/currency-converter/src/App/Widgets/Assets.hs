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
  [ Amount.amountSelect
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
    <> [ assetButton ("Duplicate " <> idxTxt) $ duplicate st optic idx,
         assetButton ("Remove " <> idxTxt) remove
       ]
  where
    idxTxt :: Text
    idxTxt = "#" <> inspect (idx + 1)

assetButton ::
  forall a action.
  ( From a String
  ) =>
  a ->
  action ->
  View action
assetButton label action =
  LayoutGrid.cell
    [ LayoutGrid.span3Desktop,
      LayoutGrid.span2Tablet,
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

duplicate ::
  Model ->
  ATraversal' Model [Asset Unique] ->
  Int ->
  Action
duplicate st optic idx =
  PushUpdate $ do
    duplicator <- newUniqueDuplicator @Text
    let updater loc el =
          if loc == idx
            then [el, duplicator el]
            else [el]
    --
    -- TODO : maybe move to general update function?
    -- Probably overhead is not to big,
    -- and this will cover all corner cases everywhere.
    --
    Misc.forceRender st
    pure
      . ChanItem 0
      $ (& cloneTraversal optic %~ ((>>= uncurry updater) . zip [0 ..]))

remove :: Action
remove =
  Noop
