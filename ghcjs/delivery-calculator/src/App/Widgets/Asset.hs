module App.Widgets.Asset
  ( assetsViewer,
  )
where

import qualified App.Misc as Misc
import App.Types
import qualified Functora.Miso.Css as Css
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Grid as Grid
import qualified Functora.Miso.Widgets.Header as Header
import qualified Material.Button as Button
import qualified Material.Dialog as Dialog
import qualified Material.IconButton as IconButton
import qualified Material.Theme as Theme

assetsViewer :: Model -> [View Action]
assetsViewer st = do
  idx <- fmap fst . zip [0 ..] $ st ^. #modelState . #stAssets
  assetViewer st idx

assetViewer :: Model -> Int -> [View Action]
assetViewer st idx =
  Header.headerViewer
    title
    [ IconButton.iconButton
        ( IconButton.config
            & IconButton.setAttributes
              [ Theme.primary
              ]
            & IconButton.setOnClick
              ( PushUpdate
                  . Instant
                  $ pure
                  . (cloneTraversal modalOptic .~ Opened)
              )
        )
        "settings"
    ]
    <> ( if st ^? cloneTraversal modalOptic /= Just Opened
          then mempty
          else
            [ Dialog.dialog
                ( Dialog.config
                    & Dialog.setOpen True
                    & Dialog.setOnClose closeAction
                )
                ( Dialog.dialogContent
                    Nothing
                    [ Grid.grid
                        mempty
                        $ Header.headerViewer title mempty
                        <> ( FieldPairs.fieldPairsEditor
                              args
                              $ FieldPairs.defOpts
                              & #optsAdvanced
                              .~ False
                           )
                        <> [ Grid.mediumCell
                              [ Button.raised
                                  ( Button.config
                                      & Button.setIcon (Just "delete_forever")
                                      & Button.setAttributes [Css.fullWidth]
                                      & Button.setOnClick
                                        ( PushUpdate
                                            . Instant
                                            $ Jsm.removeAt
                                              ( #modelState
                                                  . #stAssets
                                              )
                                              idx
                                        )
                                  )
                                  "Remove"
                              ],
                             Grid.mediumCell
                              [ Button.raised
                                  ( Button.config
                                      & Button.setOnClick closeAction
                                      & Button.setIcon (Just "save")
                                      & Button.setAttributes [Css.fullWidth]
                                  )
                                  "Save"
                              ]
                           ]
                    ]
                    mempty
                )
            ]
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
            Misc.pushActionQueue st . Instant
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
        $ pure
        . (& cloneTraversal modalOptic .~ Closed)
