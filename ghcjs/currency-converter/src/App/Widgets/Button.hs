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

data Opts action = Opts
  { optsLabel :: Maybe Text,
    optsStyle :: Set Style,
    optsOnClick :: Maybe action,
    optsLeadingIcon :: Maybe FaIcon,
    optsExtraAttributes :: [Attribute action]
  }
  deriving stock (Generic)

defOpts :: Opts action
defOpts =
  Opts
    { optsLabel = Nothing,
      optsStyle = [Primary],
      optsOnClick = Nothing,
      optsLeadingIcon = Nothing,
      optsExtraAttributes = mempty
    }

button :: Opts action -> View action
button opts =
  button_
    ( maybeToList (onClick <$> opts ^. #optsOnClick)
        <> [class_ "button", class_ "fill"]
        <> (fmap (class_ . ms . htmlStyle) . toList $ opts ^. #optsStyle)
        <> (opts ^. #optsExtraAttributes)
    )
    $ catMaybes
      [ Icon.icon Icon.defOpts <$> optsLeadingIcon opts,
        text . ms <$> optsLabel opts
      ]
