module Functora.Miso.Widgets.Chips
  ( Args (..),
    chips,
  )
where

import Functora.Miso.Prelude
import qualified Material.Chip.Filter as Filter
import qualified Material.ChipSet.Filter as Filter
import qualified Material.LayoutGrid as LayoutGrid

data Args model action = Args
  { argsModel :: model,
    argsOptic ::
      NonEmpty
        ( Unicode,
          Maybe Filter.Icon,
          ATraversal' model Bool
        ),
    argsAction :: JSM (model -> model) -> action
  }
  deriving stock (Generic)

chips :: Args model action -> View action
chips args@Args {argsOptic = items} =
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
        ( uncurry3 (chipEditor args)
            $ head items
        )
        ( uncurry3 (chipEditor args)
            <$> tail items
        )
    ]

chipEditor ::
  Args model action ->
  Unicode ->
  Maybe Filter.Icon ->
  ATraversal' model Bool ->
  Filter.Chip action
chipEditor Args {argsModel = st, argsAction = action} label icon optic =
  Filter.chip
    ( Filter.config
        & Filter.setIcon icon
        & Filter.setSelected (fromMaybe False $ st ^? cloneTraversal optic)
        & Filter.setOnChange (action $ pure (& cloneTraversal optic %~ not))
    )
    label
