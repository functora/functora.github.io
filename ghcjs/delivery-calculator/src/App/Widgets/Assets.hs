module App.Widgets.Assets
  ( assetsViewer,
  )
where

import App.Types
import Functora.Miso.Prelude
import qualified Functora.Miso.Widgets.FieldPairs as FieldPairs

assetsViewer :: Model -> [View Action]
assetsViewer st = do
  idx <- fmap fst . zip [0 ..] $ st ^. #modelState . #stAssets
  assetViewer st idx

assetViewer :: Model -> Int -> [View Action]
assetViewer st idx =
  FieldPairs.fieldPairsViewer
    FieldPairs.Args
      { FieldPairs.argsModel = st,
        FieldPairs.argsOptic = #modelState . #stAssets . ix idx . #assetFieldPairs,
        FieldPairs.argsAction = PushUpdate . Instant
      }
