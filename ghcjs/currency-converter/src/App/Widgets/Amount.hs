module App.Widgets.Amount
  ( amountSelect,
    amountSwap,
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

amountSelect ::
  Model ->
  Getter' Model Text ->
  ALens' Model (Amount Unique) ->
  ( Model -> Model
  ) ->
  View Action
amountSelect st placeholderOptic amountOptic extraOnInput =
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
    uuid = st ^. cloneLens amountOptic . #amountInput . #uniqueUuid
    input = st ^. cloneLens amountOptic . #amountInput . #uniqueValue
    output = st ^. cloneLens amountOptic . #amountOutput
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
              . #amountInput
              . #uniqueValue
              .~ inspectRatioDef output
    onInputAction txt =
      pureUpdate 300 $ \st' ->
        st'
          & cloneLens amountOptic
          . #amountInput
          . #uniqueValue
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
              . #amountInput
              . #uniqueValue
              .~ mempty
              & extraOnInput
        )

amountSwap :: View Action
amountSwap =
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
                . #amountInput
                . #uniqueValue
            baseOutput =
              st
                ^. #modelState
                . #stateTopMoney
                . #moneyAmount
                . #amountOutput
            quoteInput =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyAmount
                . #amountInput
                . #uniqueValue
            quoteOutput =
              st
                ^. #modelState
                . #stateBottomMoney
                . #moneyAmount
                . #amountOutput
         in st
              & #modelState
              . #stateTopMoney
              . #moneyAmount
              . #amountInput
              . #uniqueValue
              .~ quoteInput
              & #modelState
              . #stateTopMoney
              . #moneyAmount
              . #amountOutput
              .~ quoteOutput
              & #modelState
              . #stateBottomMoney
              . #moneyAmount
              . #amountInput
              . #uniqueValue
              .~ baseInput
              & #modelState
              . #stateBottomMoney
              . #moneyAmount
              . #amountOutput
              .~ baseOutput
              & #modelState
              . #stateTopOrBottom
              .~ Top
