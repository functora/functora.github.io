module Functora.Miso.Widgets.Select
  ( Args (..),
    Opts (..),
    defOpts,
    select,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Prelude

data Args model action t = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model t,
    argsAction :: Update model -> action,
    argsOptions :: ATraversal' model [t]
  }
  deriving stock (Generic)

data Opts t = Opts
  { optsLabel :: Maybe Unicode,
    optsEncode :: t -> Unicode,
    optsDecode :: Unicode -> t,
    optsDisplay :: t -> Unicode
  }
  deriving stock (Generic)

defOpts :: (Show t, Read t, Data t) => Opts t
defOpts =
  Opts
    { optsLabel = Nothing,
      optsEncode = from @String @Unicode . Prelude.show,
      optsDecode = Prelude.read . from @Unicode @String,
      optsDisplay = inspect
    }

select ::
  forall model action t.
  ( Eq t
  ) =>
  Opts t ->
  Args model action t ->
  View action
select opts args =
  maybe
    id
    (\x -> label_ mempty . (text x :) . singleton)
    (optsLabel opts)
    $ select_
      [ onChange $ \x ->
          action
            . PureUpdate
            $ cloneTraversal optic
            .~ optsDecode opts x
      ]
    . fmap
      ( \x ->
          option_
            ( catMaybes
                [ Just . value_ $ optsEncode opts x,
                  selected_ . (== x) <$> current
                ]
            )
            [ text
                $ (opts ^. #optsDisplay) x
            ]
      )
    . fromMaybe mempty
    $ st
    ^? cloneTraversal (args ^. #argsOptions)
  where
    st = args ^. #argsModel
    optic = args ^. #argsOptic
    action = args ^. #argsAction
    current = st ^? cloneTraversal optic
