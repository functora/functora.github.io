module Functora.Miso.Widgets.Dialog
  ( Args (..),
    Opts (..),
    defOpts,
    dialog,
    openDialog,
    closeDialog,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Language.Javascript.JSaddle as JS

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model (Unique OpenedOrClosed),
    argsAction :: Update model -> action,
    argsContent :: [View action]
  }
  deriving stock (Generic)

data Opts model = Opts
  { optsTitle :: Maybe Unicode,
    optsExtraOnOpen :: model -> model,
    optsExtraOnClose :: model -> model
  }
  deriving stock (Generic)

defOpts :: Opts model
defOpts =
  Opts
    { optsTitle = Nothing,
      optsExtraOnOpen = id,
      optsExtraOnClose = id
    }

dialog :: forall model action. Opts model -> Args model action -> [View action]
dialog opts args =
  singleton
    . nodeHtml
      "dialog"
      [ id_ $ getDialogUid (argsModel args) (argsOptic args)
      ]
    $ if args
      ^? #argsModel
      . cloneTraversal (argsOptic args)
      . #uniqueValue
      /= Just Opened
      then mempty
      else
        maybeToList
          ( fmap
              ( \x -> header_ mempty [h2_ mempty [text x]]
              )
              $ optsTitle opts
          )
          <> argsContent args
          <> [ footer_
                mempty
                [ button_
                    [onClick $ closeDialogAction opts args]
                    [text "Back"]
                ]
             ]

closeDialogAction :: Opts model -> Args model action -> action
closeDialogAction opts args =
  argsAction args
    $ PureAndImpureUpdate
      ( optsExtraOnClose opts
          . (cloneTraversal (argsOptic args) . #uniqueValue .~ Closed)
      )
      ( do
          closeDialog (argsModel args) (argsOptic args)
          pure id
      )

openDialog ::
  model ->
  ATraversal' model (Unique OpenedOrClosed) ->
  JSM ()
openDialog st optic =
  handleAny consoleLog $ do
    el <- getElementById $ getDialogUid st optic
    elExist <- ghcjsPure $ JS.isTruthy el
    when elExist . void $ el ^. JS.js0 ("showModal" :: Unicode)

closeDialog ::
  model ->
  ATraversal' model (Unique OpenedOrClosed) ->
  JSM ()
closeDialog st optic =
  handleAny consoleLog $ do
    el <- getElementById $ getDialogUid st optic
    elExist <- ghcjsPure $ JS.isTruthy el
    when elExist . void $ el ^. JS.js0 ("close" :: Unicode)

getDialogUid ::
  model ->
  ATraversal' model (Unique OpenedOrClosed) ->
  Unicode
getDialogUid st optic =
  either impureThrow id
    . decodeUtf8Strict
    . unTagged
    . htmlUid
    . fromMaybe nilUid
    $ st
    ^? cloneTraversal optic
    . #uniqueUid
