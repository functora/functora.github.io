module App.Widgets.Chips
  ( chips,
  )
where

import App.Types
import Functora.Prelude as Prelude
import qualified Material.DataTable as DataTable
import qualified Material.LayoutGrid as LayoutGrid
import qualified Material.Switch as Switch
import qualified Material.Typography as Typography
import Miso hiding (at, view)
import Miso.String hiding (cons, foldl, intercalate, null, reverse)

chips :: Model -> [(Text, ATraversal' Model Bool)] -> View Action
chips st items =
  --
  -- WIP !!!
  --
  Switch.switch
    $ Switch.config
    & Switch.setChecked
      ( fromMaybe False $ st ^? cloneTraversal optic
      )
    & Switch.setOnChange
      ( pureUpdate 0 (& cloneTraversal optic %~ not)
      )
