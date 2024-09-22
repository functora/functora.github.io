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

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model Bool,
    argsAction :: JSM (model -> model) -> action
  }
  deriving stock (Generic)

data Opts model action = Opts
  { optsDisabled :: Bool,
    optsPlaceholder :: Unicode,
    optsIcon :: Maybe Unicode
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsDisabled = False,
      optsPlaceholder = mempty,
      optsIcon = Nothing
    }

switch ::
  forall model action.
  Args model action ->
  Opts model action ->
  View action
switch Args {argsModel = st, argsOptic = optic, argsAction = action} opts =
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
          & Switch.setOnChange (action $ pure (& cloneTraversal optic %~ not))
       ]
