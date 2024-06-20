module App.Widgets.Icon
  ( Opts (..),
    defOpts,
    icon,
  )
where

import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (ms)

data Opts action = Opts
  { optsOnClick :: Maybe action,
    optsExtraAttributes :: [Attribute action]
  }
  deriving stock (Generic)

defOpts :: Opts action
defOpts =
  Opts
    { optsOnClick = Nothing,
      optsExtraAttributes = mempty
    }

icon :: Opts action -> Text -> View action
icon opts label =
  span_
    ( catMaybes
        [ Just $ class_ "icon",
          onClick <$> opts ^. #optsOnClick
        ]
        <> optsExtraAttributes opts
    )
    [ i_
        [ class_ "fas",
          class_ . ms $ "fa-" <> label
        ]
        mempty
    ]
