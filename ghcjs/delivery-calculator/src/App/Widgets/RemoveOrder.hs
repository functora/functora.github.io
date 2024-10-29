module App.Widgets.RemoveOrder (removeOrder) where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Icon as Icon

removeOrder :: Model -> [View Action]
removeOrder st =
  if null $ st ^. #modelState . #stAssets
    then mempty
    else
      button_
        [ onClick
            . PushUpdate
            . Instant
            . PureUpdate
            $ #modelRemoveOrder
            .~ Opened
        ]
        [ icon Icon.IconDelete,
          text " Remove Order"
        ]
        : Dialog.dialog
          Dialog.defOpts
            { Dialog.optsTitle = Just ("Remove order" :: Unicode),
              Dialog.optsFlexCol = False,
              Dialog.optsTitleIcon = Just Icon.IconDelete,
              Dialog.optsHeaderRight =
                const
                  [ button_
                      [type_ "reset", onClick removeAction]
                      [icon Icon.IconDelete],
                    button_
                      [type_ "submit", onClick saveAction]
                      [icon Icon.IconSave]
                  ],
              Dialog.optsFooterRight =
                const
                  [ button_
                      [type_ "reset", onClick removeAction]
                      [icon Icon.IconDelete, text " Yes"],
                    button_
                      [type_ "submit", onClick saveAction]
                      [icon Icon.IconSave, text " No"]
                  ]
            }
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = #modelRemoveOrder,
              Dialog.argsAction = PushUpdate . Instant,
              Dialog.argsContent =
                [ text "Do you really want to remove the order?"
                ]
            }
  where
    removeAction =
      PushUpdate
        . Instant
        . PureUpdate
        $ (#modelRemoveOrder .~ Closed)
        . (#modelState . #stAssets .~ mempty)
    saveAction =
      PushUpdate
        . Instant
        $ PureUpdate (#modelRemoveOrder .~ Closed)
