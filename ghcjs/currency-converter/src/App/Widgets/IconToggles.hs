module App.Widgets.IconToggles
  ( iconToggles,
  )
where

import App.Types
import Functora.Prelude
import qualified Material.IconToggle as IconToggle
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme
import Miso hiding (at, view)

iconToggles ::
  Model ->
  NonEmpty
    ( Text,
      IconToggle.Icon,
      IconToggle.Icon,
      ATraversal' Model Bool
    ) ->
  View Action
iconToggles st items =
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
    $ iconToggleWidget st
    <$> items

iconToggleWidget ::
  Model ->
  ( Text,
    IconToggle.Icon,
    IconToggle.Icon,
    ATraversal' Model Bool
  ) ->
  View Action
iconToggleWidget st (label, onIcon, offIcon, optic) =
  IconToggle.iconToggle
    ( IconToggle.config
        & IconToggle.setLabel (Just $ from @Text @String label)
        & IconToggle.setOn (fromMaybe False $ st ^? cloneTraversal optic)
        & IconToggle.setOnChange (pureUpdate 0 (& cloneTraversal optic %~ not))
        & ( if st ^? cloneTraversal optic == Just True
              then IconToggle.setAttributes [Theme.secondary]
              else id
          )
    )
    onIcon
    offIcon
