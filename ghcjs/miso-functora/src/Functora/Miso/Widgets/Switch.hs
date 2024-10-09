module Functora.Miso.Widgets.Switch
  ( Args (..),
    Opts (..),
    defOpts,
    switch,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model Bool,
    argsAction :: Update model -> action
  }
  deriving stock (Generic)

data Opts model action = Opts
  { optsDisabled :: Bool,
    optsLabel :: Maybe Unicode
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsDisabled = False,
      optsLabel = Nothing
    }

switch ::
  forall model action.
  Opts model action ->
  Args model action ->
  View action
switch opts Args {argsModel = st, argsOptic = optic, argsAction = action} =
  maybe
    id
    ( \x ->
        label_ mempty
          . (text x :)
          . (br_ mempty :)
          . singleton
    )
    (optsLabel opts)
    $ input_
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
