module App.Widgets.Icon
  ( Opts (..),
    defOpts,
    icon,
  )
where

import App.Types
import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (ms)

newtype Opts = Opts
  { optsOnClick :: Action
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsOnClick = Noop
    }

icon :: Opts -> Text -> View Action
icon opts label =
  span_
    [ class_ "icon",
      onClick $ opts ^. #optsOnClick
    ]
    [ i_ [class_ "fas", class_ . ms $ "fa-" <> label] mempty
    ]
