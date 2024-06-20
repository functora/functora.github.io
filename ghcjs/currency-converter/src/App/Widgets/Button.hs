module App.Widgets.Button
  ( Opts (..),
    defOpts,
    button,
  )
where

import qualified App.Widgets.Icon as Icon
import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (ms)

data Opts action = Opts
  { optsLabel :: Maybe Text,
    optsOnClick :: Maybe action,
    optsLeadingIcon :: Maybe Text,
    optsExtraAttributes :: [Attribute action]
  }
  deriving stock (Generic)

defOpts :: Opts action
defOpts =
  Opts
    { optsLabel = Nothing,
      optsOnClick = Nothing,
      optsLeadingIcon = Nothing,
      optsExtraAttributes = mempty
    }

button :: Opts action -> View action
button opts =
  button_
    ( maybeToList (onClick <$> opts ^. #optsOnClick)
        <> [class_ "button", class_ "fill"]
        <> (opts ^. #optsExtraAttributes)
    )
    $ catMaybes
      [ Icon.icon Icon.defOpts <$> optsLeadingIcon opts,
        text . ms <$> optsLabel opts
      ]
