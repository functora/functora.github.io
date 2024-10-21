module App.Widgets.Asset
  ( assetsViewer,
  )
where

import App.Types
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Icon as Icon

assetsViewer :: Model -> [View Action]
assetsViewer st = do
  idx <- fmap fst . zip [0 ..] $ st ^. #modelState . #stAssets
  assetViewer st idx

assetViewer :: Model -> Int -> [View Action]
assetViewer st idx =
  [ fieldset_ mempty
      $ ( legend_
            mempty
            [ text title,
              text " ",
              button_
                [ onClick
                    . PushUpdate
                    . Instant
                    . PureUpdate
                    $ cloneTraversal modalOptic
                    .~ Opened
                ]
                [ icon Icon.IconEdit,
                  text " Edit"
                ]
            ]
        )
      : FieldPairs.fieldPairsViewer FieldPairs.defOpts args
  ]
    <> ( Dialog.dialog
          ( Dialog.defOpts
              { Dialog.optsTitle = Just title,
                Dialog.optsHeaderRight =
                  const
                    [ button_
                        [ type_ "reset",
                          onClick removeAction
                        ]
                        [ icon Icon.IconDelete
                        ],
                      button_
                        [ type_ "submit",
                          onClick saveAction
                        ]
                        [ icon Icon.IconSave
                        ]
                    ],
                Dialog.optsFooterRight =
                  const
                    [ button_
                        [ type_ "reset",
                          onClick removeAction
                        ]
                        [ icon Icon.IconDelete,
                          text " Remove"
                        ],
                      button_
                        [ type_ "submit",
                          onClick saveAction
                        ]
                        [ icon Icon.IconSave,
                          text " Save"
                        ]
                    ]
              }
          )
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = modalOptic,
              Dialog.argsAction = PushUpdate . Instant,
              Dialog.argsContent =
                FieldPairs.fieldPairsEditor
                  args
                  $ FieldPairs.defOpts
                  & #optsAdvanced
                  .~ False
            }
       )
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
    removeAction =
      PushUpdate
        . Instant
        $ Jsm.removeAt (#modelState . #stAssets) idx
    saveAction =
      PushUpdate
        . Instant
        . PureUpdate
        $ cloneTraversal modalOptic
        .~ Closed
