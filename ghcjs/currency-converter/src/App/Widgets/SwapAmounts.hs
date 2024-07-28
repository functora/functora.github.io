module App.Widgets.SwapAmounts
  ( swapAmounts,
  )
where

import App.Types
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
            [ class_ "fill",
              class_ "no-print",
              Theme.secondaryBg
            ]
      )
      "Swap amounts"
  where
    onClickAction =
      pureUpdate 0 $ \st ->
        let baseInput =
              st
                ^. #modelState
                . #stDoc
                . #stDocConv
                . #stConvTopMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
            baseOutput =
              st
                ^. #modelState
                . #stDoc
                . #stDocConv
                . #stConvTopMoney
                . #moneyAmount
                . #fieldOutput
            quoteInput =
              st
                ^. #modelState
                . #stDoc
                . #stDocConv
                . #stConvBottomMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
            quoteOutput =
              st
                ^. #modelState
                . #stDoc
                . #stDocConv
                . #stConvBottomMoney
                . #moneyAmount
                . #fieldOutput
         in st
              & #modelState
              . #stDoc
              . #stDocConv
              . #stConvTopMoney
              . #moneyAmount
              . #fieldInput
              . #uniqueValue
              .~ quoteInput
              & #modelState
              . #stDoc
              . #stDocConv
              . #stConvTopMoney
              . #moneyAmount
              . #fieldOutput
              .~ quoteOutput
              & #modelState
              . #stDoc
              . #stDocConv
              . #stConvBottomMoney
              . #moneyAmount
              . #fieldInput
              . #uniqueValue
              .~ baseInput
              & #modelState
              . #stDoc
              . #stDocConv
              . #stConvBottomMoney
              . #moneyAmount
              . #fieldOutput
              .~ baseOutput
              & #modelState
              . #stDoc
              . #stDocConv
              . #stConvTopOrBottom
              .~ Top
