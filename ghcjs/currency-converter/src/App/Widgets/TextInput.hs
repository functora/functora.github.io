module App.Widgets.TextInput
  ( textInput,
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

textInput ::
  Model ->
  Text ->
  ATraversal' Model (Unique Text) ->
  View Action
textInput st placeholder optic =
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
    uuid = fromMaybe nilUuid $ st ^? cloneTraversal optic . #uniqueUuid
    onInputAction txt =
      PushUpdate $ do
        Misc.verifyUuid uuid
        pure . ChanItem 300 $ \st' ->
          st'
            & cloneTraversal optic
            . #uniqueValue
            .~ from @String @Text txt
    onCopyAction =
      PushUpdate $ do
        Misc.verifyUuid uuid
        whenJust (st ^? cloneTraversal optic . #uniqueValue)
          $ Misc.copyIntoClipboard st
        pure $ ChanItem 0 id
    onClearAction =
      PushUpdate $ do
        Misc.verifyUuid uuid
        focus
          . ms
          $ htmlUuid @Text uuid
        void
          . JS.eval @Text
          $ "var el = document.getElementById('"
          <> htmlUuid uuid
          <> "'); if (el) el.value = '';"
        pure . ChanItem 300 $ \st' ->
          st'
            & cloneTraversal optic
            . #uniqueValue
            .~ mempty