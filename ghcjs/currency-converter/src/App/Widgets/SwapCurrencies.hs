module App.Widgets.SwapCurrencies
  ( swapCurrencies,
  )
where

import App.Types
import Functora.Miso.Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme

swapCurrencies :: View Action
swapCurrencies =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    [ Button.raised
        ( Button.config
            & Button.setIcon (Just "swap_vertical_circle")
            & Button.setOnClick onClickAction
            & Button.setAttributes
              [ class_ "fill",
                class_ "no-print",
                Theme.secondaryBg
              ]
        )
        "Swap currencies"
    ]
  where
    onClickAction =
      pureUpdate 0 $ \st -> do
        let baseCurrency =
              st
                ^. #modelState
                . #stDoc
                . #stDocTopMoney
                . #moneyCurrency
                . #currencyOutput
        let quoteCurrency =
              st
                ^. #modelState
                . #stDoc
                . #stDocBottomMoney
                . #moneyCurrency
                . #currencyOutput
        pure
          $ st
          & #modelState
          . #stDoc
          . #stDocTopMoney
          . #moneyCurrency
          . #currencyOutput
          .~ quoteCurrency
          & #modelState
          . #stDoc
          . #stDocBottomMoney
          . #moneyCurrency
          . #currencyOutput
          .~ baseCurrency
          & #modelState
          . #stDoc
          . #stDocTopOrBottom
          .~ Top
