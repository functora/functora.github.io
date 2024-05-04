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
                . #stateTopMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
            baseOutput =
              st
                ^. #modelState
                . #stateTopMoney
                . #moneyAmount
                . #fieldOutput
            quoteInput =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyAmount
                . #fieldInput
                . #uniqueValue
            quoteOutput =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyAmount
                . #fieldOutput
         in st
              & #modelState
              . #stateTopMoney
              . #moneyAmount
              . #fieldInput
              . #uniqueValue
              .~ quoteInput
              & #modelState
              . #stateTopMoney
              . #moneyAmount
              . #fieldOutput
              .~ quoteOutput
              & #modelState
              . #stateBottomMoney
              . #moneyAmount
              . #fieldInput
              . #uniqueValue
              .~ baseInput
              & #modelState
              . #stateBottomMoney
              . #moneyAmount
              . #fieldOutput
              .~ baseOutput
              & #modelState
              . #stateTopOrBottom
              .~ Top
