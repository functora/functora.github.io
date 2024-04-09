module App.AmountWidget
  ( amountWidget,
    swapAmountsWidget,
  )
where

import qualified App.Misc as Misc
import App.Types
import Functora.Money
import Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

amountWidget :: Model -> TopOrBottom -> View Action
amountWidget st loc =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ TextField.outlined
        $ TextField.config
        & TextField.setType (Just "number")
        & TextField.setOnInput onInputAction
        & TextField.setLeadingIcon
          ( Just
              $ TextField.icon
                [ class_ "mdc-text-field__icon--leading",
                  intProp "tabindex" 0,
                  textProp "role" "button",
                  onClick onCopyAction
                ]
                "content_copy"
          )
        & TextField.setTrailingIcon
          ( Just
              $ TextField.icon
                [ class_ "mdc-text-field__icon--trailing",
                  intProp "tabindex" 0,
                  textProp "role" "button",
                  onClick onClearAction
                ]
                "close"
          )
        & TextField.setPlaceholder
          ( Just
              . inspectCurrencyInfo
              $ st
              ^. cloneLens moneyLens
              . #modelMoneyCurrencyInfo
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_
              . ms
              . htmlUuid @Text
              $ st
              ^. cloneLens moneyLens
              . #modelMoneyAmountUuid,
            onKeyDown $ Misc.onKeyDownAction uuid,
            onBlur onBlurAction
          ]
    ]
  where
    moneyLens = Misc.getConverterMoneyLens loc
    uuid = st ^. cloneLens moneyLens . #modelMoneyAmountUuid
    input = st ^. cloneLens moneyLens . #modelMoneyAmountInput
    output = st ^. cloneLens moneyLens . #modelMoneyAmountOutput
    valid =
      (parseMoney input == Just output)
        || (input == inspectMoneyAmount output)
    onBlurAction =
      pureUpdate 300 $ \st' ->
        if valid
          then st'
          else
            st'
              & cloneLens moneyLens
              . #modelMoneyAmountInput
              .~ inspectMoneyAmount output
    onInputAction txt =
      pureUpdate 300 $ \st' ->
        st'
          & cloneLens moneyLens
          . #modelMoneyAmountInput
          .~ from @String @Text txt
          & #modelData
          . #modelDataTopOrBottom
          .~ loc
    onCopyAction =
      PushUpdate
        ( Misc.copyIntoClipboard st
            $ st
            ^. cloneLens moneyLens
            . #modelMoneyAmountInput
        )
        ( ChanItem 0 id
        )
    onClearAction =
      PushUpdate
        ( do
            focus . ms $ htmlUuid @Text uuid
            void
              . JS.eval @Text
              $ "var el = document.getElementById('"
              <> htmlUuid uuid
              <> "'); if (el) el.value = '';"
        )
        ( ChanItem 300 $ \st' ->
            st'
              & cloneLens moneyLens
              . #modelMoneyAmountInput
              .~ mempty
              & #modelData
              . #modelDataTopOrBottom
              .~ loc
        )

swapAmountsWidget :: View Action
swapAmountsWidget =
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
              st ^. #modelData . #modelDataTopMoney . #modelMoneyAmountInput
            baseOutput =
              st ^. #modelData . #modelDataTopMoney . #modelMoneyAmountOutput
            quoteInput =
              st ^. #modelData . #modelDataBottomMoney . #modelMoneyAmountInput
            quoteOutput =
              st ^. #modelData . #modelDataBottomMoney . #modelMoneyAmountOutput
         in st
              & #modelData
              . #modelDataTopMoney
              . #modelMoneyAmountInput
              .~ quoteInput
              & #modelData
              . #modelDataTopMoney
              . #modelMoneyAmountOutput
              .~ quoteOutput
              & #modelData
              . #modelDataBottomMoney
              . #modelMoneyAmountInput
              .~ baseInput
              & #modelData
              . #modelDataBottomMoney
              . #modelMoneyAmountOutput
              .~ baseOutput
              & #modelData
              . #modelDataTopOrBottom
              .~ Top
