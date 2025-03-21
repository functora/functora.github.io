module App.Widgets.Chips
  ( chips,
  )
where

import App.Prelude
import App.Types
import qualified Material.Chip.Filter as Filter
import qualified Material.ChipSet.Filter as Filter
import qualified Material.LayoutGrid as LayoutGrid
import Miso hiding (at, view)

chips ::
  Model ->
  NonEmpty
    ( Text,
      Maybe Filter.Icon,
      ATraversal' Model Bool
    ) ->
  View Action
chips st items =
  LayoutGrid.cell
    [ LayoutGrid.span6Desktop,
      LayoutGrid.span4Tablet,
      LayoutGrid.span4Phone,
      style_
        [ ("display", "flex"),
          ("align-items", "center")
        ]
    ]
    [ Filter.chipSet
        [ style_
            [ ("display", "flex"),
              ("align-items", "center"),
              ("justify-content", "space-between")
            ]
        ]
        ( uncurry3 (chipWidget st)
            $ head items
        )
        ( uncurry3 (chipWidget st)
            <$> tail items
        )
    ]

chipWidget ::
  Model ->
  Text ->
  Maybe Filter.Icon ->
  ATraversal' Model Bool ->
  Filter.Chip Action
chipWidget st label icon optic =
  Filter.chip
    ( Filter.config
        & Filter.setSelected (fromMaybe False $ st ^? cloneTraversal optic)
        & Filter.setOnChange (pureUpdate 0 (& cloneTraversal optic %~ not))
        & Filter.setIcon icon
    )
    $ from @Text @String label
