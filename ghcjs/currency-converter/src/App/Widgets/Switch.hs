module App.Widgets.Switch
  ( Opts (..),
    defOpts,
    switch,
  )
where

import App.Prelude as Prelude
import App.Types
import qualified App.Widgets.Frame as Frame
import qualified Material.Icon as Icon
import qualified Material.Switch as Switch
import Miso hiding (at, view)

data Opts = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: Text,
    optsIcon :: Maybe Text
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
      ( fmap (Icon.icon mempty . from @Text @String)
          $ optsIcon opts
      )
    <> [ Miso.rawHtml "&nbsp;",
         Miso.text . ms $ opts ^. #optsPlaceholder,
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
