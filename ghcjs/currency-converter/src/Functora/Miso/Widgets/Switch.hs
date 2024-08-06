module Functora.Miso.Widgets.Switch
  ( Opts (..),
    defOpts,
    switch,
  )
where

import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Frame as Frame
import qualified Material.Icon as Icon
import qualified Material.Switch as Switch
import Miso hiding (at, view)

data Opts model action = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: MisoString,
    optsIcon :: Maybe MisoString,
    optsOnChange :: Maybe ((model -> model) -> action)
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty,
      optsIcon = Nothing,
      optsOnChange = Nothing
    }

switch :: model -> Opts model action -> ATraversal' model Bool -> View action
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
          & ( maybe
                id
                ( \f ->
                    Switch.setOnChange $ f (& cloneTraversal optic %~ not)
                )
                $ optsOnChange opts
            )
       ]
