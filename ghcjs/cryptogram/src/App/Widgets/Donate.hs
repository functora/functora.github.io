module App.Widgets.Donate (donate) where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Icon as Icon

donate :: Model -> [View Action]
donate st =
  ( button_
      [ onClick openWidget
      ]
      [ icon Icon.IconBitcoin,
        text " Donate"
      ]
  )
    : Dialog.dialog
      ( Dialog.defOpts
          & #optsTitle
          .~ Just ("Donate" :: Unicode)
          & #optsTitleIcon
          .~ Just Icon.IconBitcoin
      )
      Dialog.Args
        { Dialog.argsModel = st,
          Dialog.argsOptic = #modelDonate,
          Dialog.argsAction = PushUpdate,
          Dialog.argsContent =
            fmap
              ( appendAttrs
                  [ style_
                      [ ("gap", "1rem"),
                        ("display", "flex"),
                        ("text-align", "center"),
                        ("align-items", "center"),
                        ("flex-direction", "column")
                      ]
                  ]
              )
              $ FieldPairs.fieldPairsViewer
                FieldPairs.defOpts
                FieldPairs.Args
                  { FieldPairs.argsModel = st,
                    FieldPairs.argsOptic = #modelDonateViewer,
                    FieldPairs.argsAction = PushUpdate,
                    FieldPairs.argsEmitter = emitter st
                  }
        }
  where
    openWidget =
      PushUpdate
        . PureUpdate
        $ #modelDonate
        .~ Opened
