module App.Widgets.Assets
  ( assets,
  )
where

import App.Types
import qualified App.Widgets.Amount as Amount
import Functora.Money
import Functora.Prelude as Prelude
import Miso hiding (at, view)

assets :: Model -> ALens' Model [Asset Unique] -> [View Action]
assets st optic =
  zip [0 :: Int ..] (st ^. cloneLens optic)
    >>= assetsWidget st optic
    . fst

assetsWidget ::
  Model ->
  ALens' Model [Asset Unique] ->
  Int ->
  [View Action]
assetsWidget st optic idx =
  [ Amount.amountSelect
      st
      ( cloneLens optic
          . ix idx
          . #assetMoney
          . #moneyCurrency
          . #currencyOutput
          . to (inspectCurrencyInfo @Text)
      )
      ( cloneLens optic
          . ix idx
          . #assetMoney
          . #moneyAmount
      )
      id
  ]
