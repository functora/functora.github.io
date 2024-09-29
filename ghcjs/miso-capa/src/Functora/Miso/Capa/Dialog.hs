module Functora.Miso.Capa.Dialog
  ( Args (..),
    Opts (..),
    defOpts,
    dialog,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model OpenedOrClosed,
    argsAction :: Update model -> action,
    argsContent :: [View action]
  }
  deriving stock (Generic)

data Opts model = Opts
  { optsTitle :: Maybe Unicode,
    optsExtraOnClose :: model -> model
  }
  deriving stock (Generic)

defOpts :: Opts model
defOpts =
  Opts
    { optsTitle = Nothing,
      optsExtraOnClose = id
    }

dialog :: forall model action. Opts model -> Args model action -> [View action]
dialog opts args =
  if args ^? #argsModel . cloneTraversal optic /= Just Opened
    then mempty
    else
      singleton
        $ div_
          [class_ "window"]
          [ div_
              [class_ "title-bar"]
              $ catMaybes
                [ fmap
                    ( \title ->
                        div_
                          [class_ "title-bar-text"]
                          [text title]
                    )
                    ( opts ^. #optsTitle
                    ),
                  Just
                    $ div_
                      [class_ "title-bar-controls"]
                      [ button_
                          [ onClick close,
                            textProp "aria-label" "Close"
                          ]
                          mempty
                      ]
                ],
            div_
              [class_ "window-body"]
              $ (args ^. #argsContent)
              <> [button_ [onClick close] [text "Back"]]
          ]
  where
    optic :: ATraversal' model OpenedOrClosed
    optic = args ^. #argsOptic
    close :: action
    close = args ^. #argsAction $ PureUpdate $ cloneTraversal optic .~ Closed
