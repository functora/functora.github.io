module Functora.Miso.Widgets.BrowserLink
  ( Args (..),
    browserLink,
  )
where

import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Prelude as Prelude
import qualified Text.URI as URI

data Args model action = Args
  { argsLink :: Prelude.Text,
    argsLabel :: MisoString,
    argsAction :: (model -> JSM model) -> action
  }
  deriving stock (Generic)

browserLink :: Args model action -> View action
browserLink Args {argsLink = link, argsLabel = label, argsAction = action} =
  a_
    [ href_ "#!",
      onClick
        . action
        . Jsm.openBrowserPage
        . either impureThrow id
        $ URI.mkURI link
    ]
    [ text label
    ]
