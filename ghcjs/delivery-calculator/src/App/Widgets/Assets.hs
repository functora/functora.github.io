module App.Widgets.Assets
  ( assetsViewer,
  )
where

import qualified App.Misc as Misc
import App.Types
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs

assetsViewer :: Model -> [View Action]
assetsViewer st = do
  idx <- fmap fst . zip [0 ..] $ st ^. #modelState . #stAssets
  assetViewer st idx

assetViewer :: Model -> Int -> [View Action]
assetViewer st idx =
  ( FieldPairs.fieldPairsViewer args
  )
    <> ( if st
          ^? #modelState
          . #stAssets
          . ix idx
          . #assetModalState
          == Just Opened
          then
            FieldPairs.fieldPairsEditor
              args
              $ FieldPairs.defOpts
              & #optsAdvanced
              .~ False
          else mempty
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
            Misc.pushActionQueue st . Instant
        }
