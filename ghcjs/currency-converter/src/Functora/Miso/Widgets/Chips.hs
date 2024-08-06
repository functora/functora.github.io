module Functora.Miso.Widgets.Chips
  ( Opts (..),
    chips,
  )
where

import Functora.Miso.Prelude
import qualified Material.Chip.Filter as Filter
import qualified Material.ChipSet.Filter as Filter
import qualified Material.LayoutGrid as LayoutGrid

newtype Opts model action = Opts
  { optsOnChange :: (model -> model) -> action
  }
  deriving stock (Generic)

chips ::
  model ->
  Opts model action ->
  NonEmpty
    ( MisoString,
      Maybe Filter.Icon,
      ATraversal' model Bool
    ) ->
  View action
chips st opts items =
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
        ( uncurry3 (chipWidget st opts)
            $ head items
        )
        ( uncurry3 (chipWidget st opts)
            <$> tail items
        )
    ]

chipWidget ::
  model ->
  Opts model action ->
  MisoString ->
  Maybe Filter.Icon ->
  ATraversal' model Bool ->
  Filter.Chip action
chipWidget st opts label icon optic =
  Filter.chip
    ( Filter.config
        & Filter.setIcon icon
        & Filter.setSelected (fromMaybe False $ st ^? cloneTraversal optic)
        & Filter.setOnChange (optsOnChange opts (& cloneTraversal optic %~ not))
    )
    label
