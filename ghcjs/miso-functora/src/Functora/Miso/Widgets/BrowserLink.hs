module Functora.Miso.Capa.BrowserLink
  ( Args (..),
    browserLink,
  )
where

import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import Functora.Miso.Types

data Args model action = Args
  { argsLink :: URI,
    argsLabel :: Unicode,
    argsAction :: Update model -> action
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
