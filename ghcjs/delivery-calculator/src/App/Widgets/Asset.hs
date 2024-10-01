module App.Widgets.Asset
  ( assetsViewer,
  )
where

import App.Types
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Grid as Grid

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
              $ PureAndImpureUpdate
                ( cloneTraversal modalOptic
                    . #uniqueValue
                    .~ Opened
                )
                ( do
                    Dialog.openDialog st modalOptic
                    pure id
                )
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
                [ Grid.grid
                    mempty
                    $ [ h1_ mempty
                          $ [text title]
                      ]
                    <> ( FieldPairs.fieldPairsEditor
                          args
                          $ FieldPairs.defOpts
                          & #optsAdvanced
                          .~ False
                       )
                    <> [ Grid.mediumCell
                          [ button_
                              [ onClick
                                  . PushUpdate
                                  . Instant
                                  $ Jsm.removeAt
                                    ( #modelState . #stAssets
                                    )
                                    idx,
                                Css.fullWidth
                              ]
                              [ text "Remove"
                              ]
                          ],
                         Grid.mediumCell
                          [ button_
                              [ onClick closeAction,
                                Css.fullWidth
                              ]
                              [ text "Save"
                              ]
                          ]
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
        $ PureAndImpureUpdate
          ( cloneTraversal modalOptic
              . #uniqueValue
              .~ Closed
          )
          ( do
              Dialog.closeDialog st modalOptic
              pure id
          )
