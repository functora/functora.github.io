module App.Widgets.Button
  ( Opts (..),
    defOpts,
    button,
  )
where

import App.Types
import qualified App.Widgets.Icon as Icon
import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (ms)

data Opts = Opts
  { optsLabel :: Maybe Text,
    optsOnClick :: Action,
    optsLeadingIcon :: Maybe Text,
    optsExtraAttributes :: [Attribute Action]
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsLabel = Nothing,
      optsOnClick = Noop,
      optsLeadingIcon = Nothing,
      optsExtraAttributes = mempty
    }

button :: Opts -> View Action
button opts =
  button_
    ( class_ "button"
        : class_ "fill"
        : onClick (opts ^. #optsOnClick)
        : opts ^. #optsExtraAttributes
    )
    $ catMaybes
      [ Icon.icon Icon.defOpts <$> optsLeadingIcon opts,
        text . ms <$> optsLabel opts
      ]
