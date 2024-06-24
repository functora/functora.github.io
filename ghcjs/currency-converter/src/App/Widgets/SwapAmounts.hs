module App.Widgets.SwapAmounts
  ( swapAmounts,
  )
where

import App.Types
import qualified App.Widgets.Button as Button
import qualified App.Widgets.Grid as Grid
import Functora.Prelude as Prelude
import Miso hiding (view)

swapAmounts :: View Action
swapAmounts =
  Grid.mediumCell
    [ Button.button
        ( Button.defOpts
            & #optsLabel
            .~ Just "Swap amounts"
            & (#optsStyle :: Lens' (Button.Opts Action) (Set Style))
            .~ [Info, Light]
            & ( #optsOnClick ::
                  Lens' (Button.Opts Action) (Maybe Action)
              )
            .~ Just onClickAction
            & ( #optsLeadingIcon ::
                  Lens' (Button.Opts Action) (Maybe FaIcon)
              )
            .~ Just FaArrowsLeftRight
        )
    ]
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
