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
                    . PureUpdate
                    $ cloneTraversal modalOptic
                    .~ Opened
                ]
                [ icon Icon.IconEdit,
                  text " Edit"
                ]
            ]
        )
      : FieldPairs.fieldPairsViewer fieldPairsOpts args
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
              Dialog.argsAction = PushUpdate,
              Dialog.argsContent =
                failures False
                  <> FieldPairs.fieldPairsEditor args fieldPairsOpts
            }
       )
  where
    args =
      FieldPairs.Args
        { FieldPairs.argsModel = st,
          FieldPairs.argsOptic =
            #modelState . #stAssets . ix idx . #assetFieldPairs,
          FieldPairs.argsAction =
            PushUpdate,
          FieldPairs.argsEmitter = \updater -> do
            sink <- readMVar $ modelSink st
            liftIO . sink $ PushUpdate updater
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
        $ Jsm.removeAt (#modelState . #stAssets) idx
    saveAction =
      PushUpdate
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

fieldPairsOpts :: FieldPairs.Opts Model Action
fieldPairsOpts =
  FieldPairs.defOpts
    { FieldPairs.optsField = \case
        0 -> opts {Field.optsTrailingWidgets = trws}
        _ -> opts,
      FieldPairs.optsAdvanced = False
    }
  where
    opts =
      Field.defOpts
        { Field.optsExtraAttributesImage =
            [ style_ [("max-height", "10vh")]
            ]
        }
    trws input fob eod =
      if eod == Disabled
        then Field.defTrailingWidgets input fob eod
        else case fob of
          Focused | null input -> [Field.PasteWidget mempty, modal]
          Focused -> Field.defTrailingWidgets input fob eod
          Blurred -> [Field.PasteWidget hide, modal]
    modal =
      Field.ActionWidget Icon.IconShopping mempty
        . PushUpdate
        . PureUpdate
        $ #modelMarketLinks
        .~ Opened

hide :: [Attribute action]
hide = [style_ [("display", "none")]]
