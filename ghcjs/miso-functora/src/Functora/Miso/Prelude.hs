module Functora.Miso.Prelude
  ( module X,
    consoleLog,
    role_,
    form_,
  )
where

import Functora.Cfg as X
import Functora.Miso.Orphan as X ()
import Functora.Prelude as X hiding
  ( Field (..),
    cons,
    field,
  )
import Functora.Rfc2397 as X
import Functora.Uri as X
import Miso as X hiding
  ( Key,
    Text,
    URI,
    at,
    close,
    consoleLog,
    for_,
    form_,
    view,
  )
import qualified Miso

consoleLog :: (Show a, Data a) => a -> JSM ()
consoleLog = Miso.consoleLog . inspect @Unicode

role_ :: Unicode -> Attribute action
role_ = textProp "role"

form_ :: [Attribute action] -> [View action] -> View action
form_ attrs = Miso.form_ (action_ "javascript:void(0)" : attrs)
