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
  Fold Model Text ->
  ATraversal' Model (Amount Unique) ->
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
    [ TextField.filled
        $ TextField.config
        & TextField.setType (Just "number")
        & TextField.setOnInput onInputAction
        & TextField.setLabel
          ( fmap (from @Text @String) $ st ^? placeholderOptic
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
            id_ . ms $ htmlUid @Text uid,
            onKeyDown $ Misc.onKeyDownAction uid,
            onBlur onBlurAction
          ]
    ]
  where
    uid =
      fromMaybe nilUid
        $ st
        ^? cloneTraversal amountOptic
        . #amountInput
        . #uniqueUid
    getInput st' =
      st' ^? cloneTraversal amountOptic . #amountInput . #uniqueValue
    getOutput st' =
      (getInput st' >>= parseRatio)
        <|> (st' ^? cloneTraversal amountOptic . #amountOutput)
    getInputReplacement st' = do
      inp <- getInput st'
      out <- getOutput st'
      if (parseRatio inp == Just out) || (inp == inspectRatioDef out)
        then Nothing
        else Just out
    onBlurAction =
      PushUpdate $ do
        Misc.verifyUid uid
        pure . ChanItem 300 $ \st' ->
          st'
            & cloneTraversal amountOptic
            . #amountInput
            . #uniqueValue
            %~ maybe id (const . inspectRatioDef) (getInputReplacement st')
            & cloneTraversal amountOptic
            . #amountOutput
            %~ maybe id (const . id) (getOutput st')
    onInputAction txt =
      PushUpdate $ do
        Misc.verifyUid uid
        pure . ChanItem 300 $ \prev ->
          let next =
                prev
                  & cloneTraversal amountOptic
                  . #amountInput
                  . #uniqueValue
                  .~ from @String @Text txt
                  & extraOnInput
           in next
                & cloneTraversal amountOptic
                . #amountOutput
                %~ maybe id (const . id) (getOutput next)
    onCopyAction =
      PushUpdate $ do
        Misc.verifyUid uid
        whenJust (getInput st) $ Misc.copyIntoClipboard st
        pure $ ChanItem 0 id
    onClearAction =
      PushUpdate $ do
        Misc.verifyUid uid
        focus
          . ms
          $ htmlUid @Text uid
        void
          . JS.eval @Text
          $ "var el = document.getElementById('"
          <> htmlUid uid
          <> "'); if (el) el.value = '';"
        pure . ChanItem 300 $ \st' ->
          st'
            & cloneTraversal amountOptic
            . #amountInput
            . #uniqueValue
            .~ mempty
            & extraOnInput

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
