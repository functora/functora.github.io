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
import qualified Functora.Miso.Widgets.Flex as Flex
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
    optsTitleIcon :: Maybe Icon.Icon,
    optsHeaderLeft :: [View action] -> [View action],
    optsHeaderRight :: [View action] -> [View action],
    optsFooterLeft :: [View action] -> [View action],
    optsFooterRight :: [View action] -> [View action],
    optsExtraOnClose :: model -> model,
    optsExtraAttributes :: [Attribute action],
    optsFlexCol :: Bool,
    optsKeyed :: Maybe Unicode,
    optsIcon :: Icon.Icon -> View action
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsTitle = Nothing,
      optsTitleIcon = Nothing,
      optsHeaderLeft = id,
      optsHeaderRight = id,
      optsFooterLeft = id,
      optsFooterRight = id,
      optsExtraOnClose = id,
      optsExtraAttributes = mempty,
      optsFlexCol = True,
      optsKeyed = Nothing,
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
      [ maybe
          id
          (keyed . (<> "-overlay"))
          (optsKeyed opts)
          $ FixedOverlay.fixedOverlay
            [ role_ "button",
              onClick $ closeDialogAction opts args
            ],
        maybe
          id
          (keyed . (<> "-content"))
          (optsKeyed opts)
          . nodeHtml
            "dialog"
            ( boolProp "open" True
                : optsExtraAttributes opts
            )
          $ Flex.flexLeftRight
            header_
            id
            (optsHeaderLeft opts defHeaderLeft)
            (optsHeaderRight opts defHeaderRight)
          <> ( if optsFlexCol opts
                then
                  [ Flex.flexCol form_ (novalidate_ True :) $ argsContent args
                  ]
                else
                  [ Flex.flexRow form_ (novalidate_ True :) $ argsContent args
                  ]
             )
          <> Flex.flexLeftRight
            footer_
            id
            (optsFooterLeft opts mempty)
            (optsFooterRight opts defFooterRight)
      ]
  where
    opened =
      args ^? #argsModel . cloneTraversal (argsOptic args) == Just Opened
    defHeaderStyle =
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    defHeaderLeft =
      singleton
        . h3_ [defHeaderStyle]
        . intersperse (rawHtml "&nbsp;")
        $ catMaybes
          [ optsIcon opts <$> optsTitleIcon opts,
            text <$> optsTitle opts
          ]
    defHeaderRight =
      [ button_
          [onClick $ closeDialogAction opts args]
          [optsIcon opts Icon.IconClose]
      ]
    defFooterRight =
      [ button_
          [onClick $ closeDialogAction opts args]
          [ optsIcon opts Icon.IconBack,
            text " Back"
          ]
      ]

closeDialogAction :: Opts model action -> Args model action -> action
closeDialogAction opts args =
  argsAction args
    . PureUpdate
    $ optsExtraOnClose opts
    . (cloneTraversal (argsOptic args) .~ Closed)
