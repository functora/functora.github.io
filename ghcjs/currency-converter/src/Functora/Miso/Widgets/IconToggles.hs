module Functora.Miso.Widgets.IconToggles
  ( Opts (..),
    defOpts,
    iconToggles,
  )
where

import Functora.Miso.Prelude
import qualified Material.IconToggle as IconToggle
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Theme as Theme

newtype Opts model action = Opts
  { optsOnChange :: Maybe ((model -> model) -> action)
  }
  deriving stock (Generic)

defOpts :: Opts model action
defOpts =
  Opts
    { optsOnChange = Nothing
    }

iconToggles ::
  model ->
  Opts model action ->
  NonEmpty
    ( MisoString,
      IconToggle.Icon,
      IconToggle.Icon,
      ATraversal' model Bool
    ) ->
  View action
iconToggles st opts items =
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
    $ iconToggleWidget st opts
    <$> items

iconToggleWidget ::
  model ->
  Opts model action ->
  ( MisoString,
    IconToggle.Icon,
    IconToggle.Icon,
    ATraversal' model Bool
  ) ->
  View action
iconToggleWidget st opts (label, onIcon, offIcon, optic) =
  IconToggle.iconToggle
    ( IconToggle.config
        & IconToggle.setLabel (Just label)
        & IconToggle.setOn (fromMaybe False $ st ^? cloneTraversal optic)
        & ( maybe
              id
              ( \f ->
                  IconToggle.setOnChange $ f (& cloneTraversal optic %~ not)
              )
              $ optsOnChange opts
          )
        & ( if st ^? cloneTraversal optic == Just True
              then IconToggle.setAttributes [Theme.secondary]
              else id
          )
    )
    onIcon
    offIcon
