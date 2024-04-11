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
              ^. cloneLens optic
              . #moneyModelCurrency
              . #currencyModelData
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_ . ms $ htmlUuid @Text uuid,
            onKeyDown $ Misc.onKeyDownAction uuid,
            onBlur onBlurAction
          ]
    ]
  where
    optic = Misc.getConverterMoneyLens loc
    uuid = st ^. cloneLens optic . #moneyModelAmount . #amountModelUuid
    input = st ^. cloneLens optic . #moneyModelAmount . #amountModelInput
    output = st ^. cloneLens optic . #moneyModelAmount . #amountModelOutput
    valid =
      (parseRatio input == Just output)
        || (input == inspectRatioDef output)
    onBlurAction =
      pureUpdate 300 $ \st' ->
        if valid
          then st'
          else
            st'
              & cloneLens optic
              . #moneyModelAmount
              . #amountModelInput
              .~ inspectRatioDef output
    onInputAction txt =
      pureUpdate 300 $ \st' ->
        st'
          & cloneLens optic
          . #moneyModelAmount
          . #amountModelInput
          .~ from @String @Text txt
          & #modelData
          . #dataModelTopOrBottom
          .~ loc
    onCopyAction =
      PushUpdate
        ( Misc.copyIntoClipboard st
            $ st
            ^. cloneLens optic
            . #moneyModelAmount
            . #amountModelInput
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
              & cloneLens optic
              . #moneyModelAmount
              . #amountModelInput
              .~ mempty
              & #modelData
              . #dataModelTopOrBottom
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
              st
                ^. #modelData
                . #dataModelTopMoney
                . #moneyModelAmount
                . #amountModelInput
            baseOutput =
              st
                ^. #modelData
                . #dataModelTopMoney
                . #moneyModelAmount
                . #amountModelOutput
            quoteInput =
              st
                ^. #modelData
                . #dataModelBottomMoney
                . #moneyModelAmount
                . #amountModelInput
            quoteOutput =
              st
                ^. #modelData
                . #dataModelBottomMoney
                . #moneyModelAmount
                . #amountModelOutput
         in st
              & #modelData
              . #dataModelTopMoney
              . #moneyModelAmount
              . #amountModelInput
              .~ quoteInput
              & #modelData
              . #dataModelTopMoney
              . #moneyModelAmount
              . #amountModelOutput
              .~ quoteOutput
              & #modelData
              . #dataModelBottomMoney
              . #moneyModelAmount
              . #amountModelInput
              .~ baseInput
              & #modelData
              . #dataModelBottomMoney
              . #moneyModelAmount
              . #amountModelOutput
              .~ baseOutput
              & #modelData
              . #dataModelTopOrBottom
              .~ Top
