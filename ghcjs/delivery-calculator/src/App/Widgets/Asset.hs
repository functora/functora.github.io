module App.Widgets.Asset
  ( assetsViewer,
  )
where

import App.Types
import qualified Functora.Miso.Jsm as Jsm
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.Dialog as Dialog
import qualified Functora.Miso.Widgets.Field as Field
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs
import qualified Functora.Miso.Widgets.Icon as Icon

assetsViewer :: Model -> [View Action]
assetsViewer st = do
  let assets = st ^. #modelState . #stAssets
  idx <- fmap fst $ zip [0 ..] assets
  assetViewer st idx $ "asset-" <> inspect (length assets - idx - 1)

assetViewer :: Model -> Int -> Unicode -> [View Action]
assetViewer st idx opfsPrefix =
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
      : FieldPairs.fieldPairsViewer (fieldPairsOpts opfsPrefix) args
  ]
    <> ( Dialog.dialog
          Dialog.defOpts
            { Dialog.optsTitle = Just title,
              Dialog.optsExtraOnClose = saveUpdate,
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
                      [icon Icon.IconDelete, text " Remove"],
                    button_
                      [type_ "submit", onClick saveAction]
                      [icon Icon.IconSave, text " Save"]
                  ]
            }
          Dialog.Args
            { Dialog.argsModel = st,
              Dialog.argsOptic = modalOptic,
              Dialog.argsAction = PushUpdate . Instant,
              Dialog.argsContent =
                failures False
                  <> FieldPairs.fieldPairsEditor
                    args
                    (fieldPairsOpts opfsPrefix)
                      { FieldPairs.optsAdvanced = False
                      }
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
        $ PureUpdate saveUpdate
    saveUpdate =
      ( #modelState
          . #stAssets
          . ix idx
          . #assetMustVerify
          .~ True
      )
        . if null $ failures True
          then cloneTraversal modalOptic .~ Closed
          else cloneTraversal modalOptic .~ Opened
    failures forceVerify =
      maybe
        mempty
        ( verifyAsset
            . ( if forceVerify
                  then #assetMustVerify .~ True
                  else id
              )
        )
        $ st
        ^? #modelState
        . #stAssets
        . ix idx

fieldPairsOpts :: Unicode -> FieldPairs.Opts model action
fieldPairsOpts opfsPrefix =
  FieldPairs.defOpts
    { FieldPairs.optsField =
        Field.defOpts
          { Field.optsExtraAttributesImage =
              [ style_ [("max-height", "10vh")]
              ]
          },
      FieldPairs.optsOpfsName =
        Just
          $ (opfsPrefix <>)
          . ("-field-" <>)
          . inspect
    }
