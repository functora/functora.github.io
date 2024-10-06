module App.Widgets.Asset
  ( assetsViewer,
  )
where

import App.Types
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs

assetsViewer :: Model -> [View Action]
assetsViewer st = do
  idx <- fmap fst . zip [0 ..] $ st ^. #modelState . #stAssets
  assetViewer st idx

assetViewer :: Model -> Int -> [View Action]
assetViewer st idx =
  [ h1_
      mempty
      [ text title,
        button_
          [ onClick
              . PushUpdate
              . Instant
              . PureUpdate
              $ cloneTraversal modalOptic
              .~ Opened
          ]
          [ text "settings"
          ]
      ]
  ]
    <> ( Dialog.dialog
          ( Dialog.defOpts
              & #optsTitle
              .~ Just title
          )
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = modalOptic,
              Dialog.argsAction = PushUpdate . Instant,
              Dialog.argsContent =
                FieldPairs.fieldPairsEditor
                  args
                  ( FieldPairs.defOpts & #optsAdvanced .~ False
                  )
                  <> [ button_
                        [ onClick
                            . PushUpdate
                            . Instant
                            $ Jsm.removeAt
                              ( #modelState . #stAssets
                              )
                              idx
                        ]
                        [ text "Remove"
                        ],
                       button_
                        [ onClick closeAction
                        ]
                        [ text "Save"
                        ]
                     ]
            }
       )
    <> FieldPairs.fieldPairsViewer args
  where
    args =
      FieldPairs.Args
        { FieldPairs.argsModel = st,
          FieldPairs.argsOptic =
            #modelState . #stAssets . ix idx . #assetFieldPairs,
          FieldPairs.argsAction =
            PushUpdate . Instant,
          FieldPairs.argsEmitter =
            pushActionQueue st . Instant
        }
    title =
      "Item #" <> inspect (idx + 1)
    modalOptic =
      #modelState
        . #stAssets
        . ix idx
        . #assetModalState
    closeAction =
      PushUpdate
        . Instant
        . PureUpdate
        $ cloneTraversal modalOptic
        .~ Closed
