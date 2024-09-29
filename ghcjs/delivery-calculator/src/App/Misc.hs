module App.Misc
  ( pushActionQueue,
  )
where

import App.Types
import Functora.Miso.Prelude

pushActionQueue ::
  ( MonadIO m
  ) =>
  Model ->
  InstantOrDelayed (Update Model) ->
  m ()
pushActionQueue st =
  liftIO
    . atomically
    . writeTChan (st ^. #modelProducerQueue)
