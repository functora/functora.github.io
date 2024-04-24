module App.Widgets.TextInput
  ( Opts (..),
    opts,
    textInput,
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

data Opts = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: Text
  }
  deriving stock (Eq, Ord, Show, Data, Generic)

opts :: Opts
opts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty
    }

textInput ::
  Model ->
  ATraversal' Model (Unique Text) ->
  Opts ->
  View Action
textInput st optic options =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ TextField.filled
        $ TextField.config
        & TextField.setType (Just "text")
        & TextField.setOnInput onInputAction
        & TextField.setDisabled (options ^. #optsDisabled)
        & TextField.setLeadingIcon
          ( Just
              $ TextField.icon
                [ class_ "mdc-text-field__icon--leading",
                  style_ [("pointer-events", "auto")],
                  textProp "role" "button",
                  intProp "tabindex" 0,
                  onClick onCopyAction
                ]
                "content_copy"
          )
        & TextField.setTrailingIcon
          ( if options ^. #optsDisabled
              then Nothing
              else
                Just
                  $ TextField.icon
                    [ class_ "mdc-text-field__icon--trailing",
                      intProp "tabindex" 0,
                      textProp "role" "button",
                      onClick onClearAction
                    ]
                    "close"
          )
        & TextField.setLabel
          ( Just . from @Text @String $ options ^. #optsPlaceholder
          )
        & TextField.setAttributes
          [ class_ "fill",
            id_ . ms $ htmlUid @Text uid,
            onKeyDown $ Misc.onKeyDownAction uid
          ]
    ]
  where
    uid = fromMaybe nilUid $ st ^? cloneTraversal optic . #uniqueUid
    onInputAction txt =
      PushUpdate $ do
        Misc.verifyUid uid
        pure . ChanItem 300 $ \st' ->
          st'
            & cloneTraversal optic
            . #uniqueValue
            .~ from @String @Text txt
    onCopyAction =
      PushUpdate $ do
        Misc.verifyUid uid
        whenJust (st ^? cloneTraversal optic . #uniqueValue)
          $ Misc.copyIntoClipboard st
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
            & cloneTraversal optic
            . #uniqueValue
            .~ mempty
