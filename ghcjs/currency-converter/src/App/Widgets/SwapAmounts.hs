module App.Widgets.SwapAmounts
  ( swapAmounts,
  )
where

import App.Types
import qualified Functora.Miso.Css as Css
import Functora.Miso.Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme

swapAmounts :: View Action
swapAmounts =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    . (: mempty)
    $ Button.raised
      ( Button.config
          & Button.setIcon (Just "swap_horizontal_circle")
          & Button.setOnClick onClickAction
          & Button.setAttributes
            [ Css.fullWidth,
              class_ "no-print",
              Theme.secondaryBg
            ]
      )
      "Swap amounts"
  where
    onClickAction =
      PushUpdate . Instant $ \st -> do
        let baseInput =
              st
                ^. #modelState
                . #stDoc
                . #stDocTopMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
        let baseOutput =
              st
                ^. #modelState
                . #stDoc
                . #stDocTopMoney
                . #moneyAmount
                . #fieldOutput
        let quoteInput =
              st
                ^. #modelState
                . #stDoc
                . #stDocBottomMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
        let quoteOutput =
              st
                ^. #modelState
                . #stDoc
                . #stDocBottomMoney
                . #moneyAmount
                . #fieldOutput
        pure
          $ st
          & #modelState
          . #stDoc
          . #stDocTopMoney
          . #moneyAmount
          . #fieldInput
          . #uniqueValue
          .~ quoteInput
          & #modelState
          . #stDoc
          . #stDocTopMoney
          . #moneyAmount
          . #fieldOutput
          .~ quoteOutput
          & #modelState
          . #stDoc
          . #stDocBottomMoney
          . #moneyAmount
          . #fieldInput
          . #uniqueValue
          .~ baseInput
          & #modelState
          . #stDoc
          . #stDocBottomMoney
          . #moneyAmount
          . #fieldOutput
          .~ baseOutput
          & #modelState
          . #stDoc
          . #stDocTopOrBottom
          .~ Top
