module Functora.Miso.Widgets.IconToggles
  ( Args (..),
    iconToggles,
  )
where

import Functora.Miso.Prelude
import qualified Material.IconToggle as IconToggle
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme

data Args model action = Args
  { argsModel :: model,
    argsOptic ::
      NonEmpty
        ( Unicode,
          IconToggle.Icon,
          IconToggle.Icon,
          ATraversal' model Bool
        ),
    argsAction :: JSM (model -> model) -> action
  }
  deriving stock (Generic)

iconToggles :: Args model action -> View action
iconToggles args@Args {argsOptic = items} =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      style_
        [ ("display", "flex"),
          ("align-items", "center"),
          ("justify-content", "space-between")
        ]
    ]
    . toList
    $ iconToggleWidget args
    <$> items

iconToggleWidget ::
  Args model action ->
  ( Unicode,
    IconToggle.Icon,
    IconToggle.Icon,
    ATraversal' model Bool
  ) ->
  View action
iconToggleWidget
  Args
    { argsModel = st,
      argsAction = action
    }
  (label, onIcon, offIcon, optic) =
    IconToggle.iconToggle
      ( IconToggle.config
          & IconToggle.setLabel (Just label)
          & IconToggle.setOn (fromMaybe False $ st ^? cloneTraversal optic)
          & IconToggle.setOnChange (action $ pure (& cloneTraversal optic %~ not))
          & ( if st ^? cloneTraversal optic == Just True
                then IconToggle.setAttributes [Theme.secondary]
                else id
            )
      )
      onIcon
      offIcon
