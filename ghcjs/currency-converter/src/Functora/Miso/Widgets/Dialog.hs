module Functora.Miso.Widgets.Dialog
  ( Args (..),
    dialog,
  )
where

import Functora.Miso.Prelude
import Functora.Miso.Types
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog

data Args model action = Args
  { argsModel :: model,
    argsOptic :: ATraversal' model OpenedOrClosed,
    argsAction :: (model -> JSM model) -> action,
    argsContent :: [View action]
  }
  deriving stock (Generic)

dialog :: forall model action. Args model action -> [View action]
dialog args =
  if args ^? #argsModel . cloneTraversal optic /= Just Opened
    then mempty
    else
      [ Dialog.dialog
          ( Dialog.config
              & Dialog.setOpen True
              & action Dialog.setOnClose
          )
          ( Dialog.dialogContent
              Nothing
              [ Grid.grid
                  mempty
                  $ (args ^. #argsContent)
                  <> [ Grid.bigCell
                        $ Button.raised
                          ( Button.config
                              & action Button.setOnClick
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
    optic :: ATraversal' model OpenedOrClosed
    optic = args ^. #argsOptic
    action :: (action -> f action -> f action) -> f action -> f action
    action = ($ args ^. #argsAction $ pure . (& cloneTraversal optic .~ Closed))
