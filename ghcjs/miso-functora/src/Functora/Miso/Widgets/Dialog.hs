module Functora.Miso.Widgets.Dialog
  ( Args (..),
    Opts (..),
    defOpts,
    dialog,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.FixedOverlay as FixedOverlay
import qualified Functora.Miso.Widgets.Icon as Icon

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model OpenedOrClosed,
    argsAction :: Update model -> action,
    argsContent :: [View action]
  }
  deriving stock (Generic)

data Opts model action = Opts
  { optsTitle :: Maybe Unicode,
    optsHeaderLeft :: [View action] -> [View action],
    optsHeaderRight :: [View action] -> [View action],
    optsFooterLeft :: [View action] -> [View action],
    optsFooterRight :: [View action] -> [View action],
    optsExtraOnClose :: model -> model,
    optsIcon :: Icon.Icon -> View action
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsTitle = Nothing,
      optsHeaderLeft = id,
      optsHeaderRight = id,
      optsFooterLeft = id,
      optsFooterRight = id,
      optsExtraOnClose = id,
      optsIcon = Icon.icon @Icon.Fa
    }

dialog ::
  forall model action.
  Opts model action ->
  Args model action ->
  [View action]
dialog opts args =
  if not opened
    then mempty
    else
      singleton
        . FixedOverlay.fixedOverlay
        . singleton
        . nodeHtml "dialog" [boolProp "open" True]
        $ newFlex
          header_
          id
          (optsHeaderLeft opts defHeaderLeft)
          (optsHeaderRight opts defHeaderRight)
        <> argsContent args
        <> newFlex
          footer_
          id
          (optsFooterLeft opts defFooterRight)
          (optsFooterRight opts mempty)
  where
    opened =
      args ^? #argsModel . cloneTraversal (argsOptic args) == Just Opened
    defHeaderLeft =
      maybeToList
        . fmap
          ( h1_
              [ style_
                  [ ("margin", "0"),
                    ("display", "flex"),
                    ("align-items", "center")
                  ]
              ]
              . singleton
              . text
          )
        $ optsTitle opts
    defHeaderRight =
      [ button_
          [onClick $ closeDialogAction opts args]
          [optsIcon opts Icon.IconClose]
      ]
    defFooterRight =
      [ button_
          [onClick $ closeDialogAction opts args]
          [text "Back"]
      ]

newFlex ::
  ([Attribute action] -> [View action] -> View action) ->
  ([Attribute action] -> [Attribute action]) ->
  [View action] ->
  [View action] ->
  [View action]
newFlex newTag newAttr lhs rhs =
  if null lhs && null rhs
    then mempty
    else
      singleton
        . newTag
          ( newAttr
              [ style_
                  [ ("display", "flex"),
                    ("flex-wrap", "wrap"),
                    ("flex-direction", "row"),
                    ("justify-content", "space-between")
                  ]
              ]
          )
        $ lhs
        <> [ div_
              [ style_
                  [ ("flex-grow", "1")
                  ]
              ]
              mempty
           ]
        <> rhs

closeDialogAction :: Opts model action -> Args model action -> action
closeDialogAction opts args =
  argsAction args
    . PureUpdate
    $ optsExtraOnClose opts
    . (cloneTraversal (argsOptic args) .~ Closed)
