module Functora.Miso.Capa.BrowserLink
  ( Args (..),
    browserLink,
  )
where

import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude

data Args model action = Args
  { argsLink :: URI,
    argsLabel :: Unicode,
    argsAction :: (model -> JSM model) -> action
  }
  deriving stock (Generic)

browserLink :: Args model action -> View action
browserLink Args {argsLink = link, argsLabel = label, argsAction = action} =
  a_
    [ href_ "#!",
      onClick . action $ Jsm.openBrowserPage link
    ]
    [ text label
    ]
