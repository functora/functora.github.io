module App.Widgets.Modal
  ( Opts (..),
    defOpts,
    modal,
  )
where

import App.Types
import Functora.Prelude as Prelude
import Miso hiding (view)

newtype Opts = Opts
  { optsExtraOnClose :: Model -> Model
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsExtraOnClose = id
    }

modal ::
  Model ->
  Opts ->
  ATraversal' Model OpenedOrClosed ->
  [View Action] ->
  View Action
modal st opts optic content =
  div_
    ( class_ "modal" : opened
    )
    [ div_ [class_ "modal-background", onClick closed] mempty,
      div_ [class_ "modal-content box"] content,
      button_
        [ class_ "modal-close is-large",
          textProp "aria-label" "close",
          onClick closed
        ]
        mempty
    ]
  where
    opened =
      if st ^? cloneTraversal optic == Just Opened
        then [class_ "is-active"]
        else mempty
    closed =
      pureUpdate 0
        $ (opts ^. #optsExtraOnClose)
        . (& cloneTraversal optic .~ Closed)
