module App.Widgets.ShareApp (shareApp) where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Icon as Icon

shareApp :: Model -> [View Action]
shareApp st =
  if not . null $ st ^. #modelState . #stAssets
    then mempty
    else
      [ button_
          [ onClick openWidget
          ]
          [ icon Icon.IconShare,
            text " Share app"
          ]
      ]
        <> Dialog.dialog
          ( Dialog.defOpts
              & #optsTitle
              .~ Just ("Share app" :: Unicode)
              & #optsTitleIcon
              .~ Just Icon.IconShare
          )
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = #modelShareApp,
              Dialog.argsAction = PushUpdate,
              Dialog.argsContent =
                fmap
                  ( appendAttrs
                      [ style_
                          [ ("gap", "1rem"),
                            ("display", "flex"),
                            ("align-items", "center"),
                            ("flex-direction", "column")
                          ]
                      ]
                  )
                  $ FieldPairs.fieldPairsViewer
                    FieldPairs.defOpts
                    FieldPairs.Args
                      { FieldPairs.argsModel = st,
                        FieldPairs.argsOptic = #modelUriViewer,
                        FieldPairs.argsAction = PushUpdate,
                        FieldPairs.argsEmitter = emitter st
                      }
            }
  where
    openWidget =
      PushUpdate
        . PureUpdate
        $ #modelShareApp
        .~ Opened
