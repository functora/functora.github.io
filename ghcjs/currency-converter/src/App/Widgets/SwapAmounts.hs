module App.Widgets.SwapAmounts
  ( swapAmounts,
  )
where

import App.Types
import Functora.Prelude as Prelude
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import Miso hiding (view)

swapAmounts :: View Action
swapAmounts =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span2Phone
    ]
    . (: mempty)
    $ Button.raised
      ( Button.setOnClick onClickAction
          . Button.setAttributes
            [ class_ "fill",
              Theme.secondaryBg
            ]
          $ Button.config
      )
      "Swap amounts"
  where
    onClickAction =
      pureUpdate 0 $ \st ->
        let baseInput =
              st
                ^. #modelState
                . #stConv
                . #stConvTopMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
            baseOutput =
              st
                ^. #modelState
                . #stConv
                . #stConvTopMoney
                . #moneyAmount
                . #fieldOutput
            quoteInput =
              st
                ^. #modelState
                . #stConv
                . #stConvBottomMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
            quoteOutput =
              st
                ^. #modelState
                . #stConv
                . #stConvBottomMoney
                . #moneyAmount
                . #fieldOutput
         in st
              & #modelState
              . #stConv
              . #stConvTopMoney
              . #moneyAmount
              . #fieldInput
              . #uniqueValue
              .~ quoteInput
              & #modelState
              . #stConv
              . #stConvTopMoney
              . #moneyAmount
              . #fieldOutput
              .~ quoteOutput
              & #modelState
              . #stConv
              . #stConvBottomMoney
              . #moneyAmount
              . #fieldInput
              . #uniqueValue
              .~ baseInput
              & #modelState
              . #stConv
              . #stConvBottomMoney
              . #moneyAmount
              . #fieldOutput
              .~ baseOutput
              & #modelState
              . #stConv
              . #stConvTopOrBottom
              .~ Top
