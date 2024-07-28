module App.Widgets.Switch
  ( Opts (..),
    defOpts,
    switch,
  )
where

import App.Types
import qualified App.Widgets.Frame as Frame
import Functora.Miso.Prelude
import qualified Material.Icon as Icon
import qualified Material.Switch as Switch
import Miso hiding (at, view)

data Opts = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: MisoString,
    optsIcon :: Maybe MisoString
  }
  deriving stock (Generic)

defOpts :: Opts
defOpts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty,
      optsIcon = Nothing
    }

switch :: Model -> Opts -> ATraversal' Model Bool -> View Action
switch st opts optic =
  Frame.frame
    $ maybeToList
      ( fmap (Icon.icon mempty)
          $ optsIcon opts
      )
    <> [ Miso.rawHtml "&nbsp;",
         Miso.text $ opts ^. #optsPlaceholder,
         Miso.rawHtml "&nbsp;&nbsp;",
         Switch.switch
          $ Switch.config
          & Switch.setChecked
            ( fromMaybe False $ st ^? cloneTraversal optic
            )
          & Switch.setOnChange
            ( pureUpdate 0 (& cloneTraversal optic %~ not)
            )
       ]
