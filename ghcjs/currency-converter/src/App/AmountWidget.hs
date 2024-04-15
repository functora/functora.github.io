module App.AmountWidget
  ( amountWidget,
    swapAmountsWidget,
  )
where

import qualified App.Misc as Misc
import App.Types
import Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Material.Button as Button
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.TextField as TextField
import qualified Material.Theme as Theme
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

amountWidget ::
  Model ->
  Getter' Model Text ->
  ALens' Model (Unique Amount) ->
  ( Model -> Model
  ) ->
  View Action
amountWidget st placeholderOptic amountOptic extraOnInput =
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
        & TextField.setPlaceholder
          ( Just
              . from @Text @String
              $ st
              ^. placeholderOptic
          )
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
        & TextField.setAttributes
          [ class_ "fill",
            id_ . ms $ htmlUuid @Text uuid,
            onKeyDown $ Misc.onKeyDownAction uuid,
            onBlur onBlurAction
          ]
    ]
  where
    uuid = st ^. cloneLens amountOptic . #uniqueUuid
    input = st ^. cloneLens amountOptic . #uniqueValue . #amountInput
    output = st ^. cloneLens amountOptic . #uniqueValue . #amountOutput
    valid =
      (parseRatio input == Just output)
        || (input == inspectRatioDef output)
    onBlurAction =
      pureUpdate 300 $ \st' ->
        if valid
          then st'
          else
            st'
              & cloneLens amountOptic
              . #uniqueValue
              . #amountInput
              .~ inspectRatioDef output
    onInputAction txt =
      pureUpdate 300 $ \st' ->
        st'
          & cloneLens amountOptic
          . #uniqueValue
          . #amountInput
          .~ from @String @Text txt
          & extraOnInput
    onCopyAction =
      PushUpdate
        ( Misc.copyIntoClipboard st input
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
              & cloneLens amountOptic
              . #uniqueValue
              . #amountInput
              .~ mempty
              & extraOnInput
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
                ^. #modelState
                . #stateTopMoney
                . #moneyAmount
                . #uniqueValue
                . #amountInput
            baseOutput =
              st
                ^. #modelState
                . #stateTopMoney
                . #moneyAmount
                . #uniqueValue
                . #amountOutput
            quoteInput =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyAmount
                . #uniqueValue
                . #amountInput
            quoteOutput =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyAmount
                . #uniqueValue
                . #amountOutput
         in st
              & #modelState
              . #stateTopMoney
              . #moneyAmount
              . #uniqueValue
              . #amountInput
              .~ quoteInput
              & #modelState
              . #stateTopMoney
              . #moneyAmount
              . #uniqueValue
              . #amountOutput
              .~ quoteOutput
              & #modelState
              . #stateBottomMoney
              . #moneyAmount
              . #uniqueValue
              . #amountInput
              .~ baseInput
              & #modelState
              . #stateBottomMoney
              . #moneyAmount
              . #uniqueValue
              . #amountOutput
              .~ baseOutput
              & #modelState
              . #stateTopOrBottom
              .~ Top
