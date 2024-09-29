module Functora.Miso.Prelude
  ( module X,
    consoleLog,
  )
where

import Functora.Cfg as X
import Functora.Miso.Orphan as X ()
import Functora.Prelude as X hiding
  ( Field (..),
    cons,
    field,
  )
import Miso as X hiding
  ( Key,
    Text,
    URI,
    at,
    close,
    consoleLog,
    for_,
    view,
  )
import qualified Miso

consoleLog :: (Show a, Data a) => a -> JSM ()
consoleLog = Miso.consoleLog . inspect @Unicode
