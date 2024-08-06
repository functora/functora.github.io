module Functora.Miso.Widgets.Dialog
  ( Opts (..),
    dialog,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog

newtype Opts model action = Opts
  { optsOnClose :: (model -> model) -> action
  }
  deriving stock (Generic)

dialog ::
  forall model action.
  model ->
  Opts model action ->
  ATraversal' model OpenedOrClosed ->
  [View action] ->
  [View action]
dialog st opts optic content =
  if st ^? cloneTraversal optic /= Just Opened
    then mempty
    else
      [ Dialog.dialog
          ( Dialog.config
              & Dialog.setOpen True
              & onClose Dialog.setOnClose
          )
          ( Dialog.dialogContent
              Nothing
              [ Grid.grid
                  mempty
                  $ content
                  <> [ Grid.bigCell
                        $ Button.raised
                          ( Button.config
                              & onClose Button.setOnClick
                              & Button.setIcon (Just "arrow_back")
                              & Button.setAttributes [class_ "fill"]
                          )
                          "Back"
                     ]
              ]
              mempty
          )
      ]
  where
    onClose :: (action -> f action -> f action) -> f action -> f action
    onClose = ($ optsOnClose opts (& cloneTraversal optic .~ Closed))
