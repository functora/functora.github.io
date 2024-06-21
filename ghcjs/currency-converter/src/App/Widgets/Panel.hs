module App.Widgets.Panel
  ( Opts (..),
    defOpts,
    panel,
  )
where

import Functora.Prelude as Prelude
import Miso hiding (view)
import Miso.String (ms)

data Opts action = Opts
  { optsActive :: Bool,
    optsContent :: [View action]
  }
  deriving stock (Generic)

defOpts :: Opts action
defOpts =
  Opts
    { optsActive = False,
      optsContent = mempty
    }

panel :: Maybe Text -> NonEmpty (Opts action) -> View action
panel label items =
  article_ [class_ "panel"]
    $ maybeToList
      ( label <&> \txt -> p_ [class_ "panel-heading"] [text $ ms txt]
      )
    <> fmap panelItem (toList items)

panelItem :: Opts action -> View action
panelItem opts =
  div_
    ( [class_ "panel-block"]
        <> if opts ^. #optsActive
          then [class_ "is-active"]
          else mempty
    )
    ( opts ^. #optsContent
    )
