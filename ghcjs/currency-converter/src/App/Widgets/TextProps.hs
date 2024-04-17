module App.Widgets.TextProps
  ( textProps,
  )
where

import App.Types
import Functora.Prelude as Prelude
import qualified Material.LayoutGrid as LayoutGrid
import Miso hiding (view)

textProps :: Model -> ALens' Model [TextProp Unique] -> View Action
textProps _ _ =
  LayoutGrid.layoutGrid
    [ LayoutGrid.alignMiddle
    ]
    [ LayoutGrid.inner
        mempty
        mempty
    ]
