module Functora.Miso.Widgets.Switch
  ( Opts (..),
    defOpts,
    switch,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Flex as Flex
import Miso hiding (at, view)

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model Bool,
    argsAction :: Update model -> action
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
  Opts model action ->
  Args model action ->
  View action
switch opts Args {argsModel = st, argsOptic = optic, argsAction = action} =
  Flex.flex div_ id
    $ maybeToList
      ( fmap (\icon -> i_ [class_ icon] mempty)
          $ optsIcon opts
      )
    <> [ Miso.rawHtml "&nbsp;",
         Miso.text $ opts ^. #optsPlaceholder,
         Miso.rawHtml "&nbsp;&nbsp;",
         input_
          [ type_ "checkbox",
            disabled_
              $ opts
              ^. #optsDisabled,
            checked_
              . fromMaybe False
              $ st
              ^? cloneTraversal optic,
            onChange
              . const
              . action
              . PureUpdate
              $ cloneTraversal optic
              %~ not
          ]
       ]
