module App.TextWidget
  ( textWidget,
  )
where

import qualified App.Misc as Misc
import App.Types
import Functora.Prelude as Prelude
import qualified Language.Javascript.JSaddle as JS
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.TextField as TextField
import Miso hiding (view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

textWidget ::
  Model ->
  Text ->
  ALens' Model (Unique Text) ->
  View Action
textWidget st placeholder optic =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ TextField.outlined
        $ TextField.config
        & TextField.setType (Just "text")
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
              $ from @Text @String placeholder
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_ . ms $ htmlUuid @Text uuid,
            onKeyDown $ Misc.onKeyDownAction uuid
          ]
    ]
  where
    uuid = st ^. cloneLens optic . #uniqueUuid
    onInputAction txt =
      pureUpdate 300 $ \st' ->
        st'
          & cloneLens optic
          . #uniqueValue
          .~ from @String @Text txt
    onCopyAction =
      PushUpdate
        ( Misc.copyIntoClipboard st
            $ st
            ^. cloneLens optic
            . #uniqueValue
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
              . #uniqueValue
              .~ mempty
        )
