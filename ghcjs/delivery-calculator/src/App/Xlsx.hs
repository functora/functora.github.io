module App.Xlsx
  ( newXlsx,
    xlsxFile,
    xlsxMime,
  )
where

import App.Types
import Codec.Xlsx
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Functora.Miso.Prelude hiding ((^.), _Just)
import Lens.Micro hiding (each, to)

newXlsx :: St Unique -> Map Unicode Rfc2397 -> BL.ByteString
newXlsx st imgs = xlsx
  where
    xlsx =
      fromXlsx 0
        $ def
        & atSheet "Delivery Calculator" ?~ sheet
    sheet =
      def
        & #wsDrawing ?~ Drawing mempty
        & addHeader st
        & flip
          (foldl $ addRow imgs)
          ( zip [2 ..]
              $ fmap
                (^.. #assetFieldPairs . each . #fieldPairValue)
                (st ^. #stAssets)
          )

addHeader :: St Unique -> Worksheet -> Worksheet
addHeader st sheet =
  case sortOn length headers of
    [] -> sheet
    (rowVal : _) ->
      foldl
        ( \acc (colIdx, colVal) ->
            acc
              & cellValueAt (1, colIdx) ?~ CellText colVal
        )
        sheet
        $ zip [1 ..] rowVal
  where
    headers :: [[Unicode]]
    headers =
      fmap
        ( ^..
            #assetFieldPairs
              . each
              . #fieldPairKey
              . #fieldOutput
        )
        $ stAssets st

addRow ::
  Map Unicode Rfc2397 ->
  Worksheet ->
  (RowIndex, [Field DynamicField Unique]) ->
  Worksheet
addRow imgs sheet (rowIdx, rowVal) =
  foldl
    ( \acc (colIdx, colVal) ->
        addCol imgs acc rowIdx colIdx colVal
    )
    sheet
    $ zip [1 ..] rowVal

addCol ::
  Map Unicode Rfc2397 ->
  Worksheet ->
  RowIndex ->
  ColumnIndex ->
  Field DynamicField Unique ->
  Worksheet
addCol imgs sheet rowIdx colIdx field =
  if fieldType field /= FieldTypeImage
    then
      sheet
        & cellValueAt (rowIdx, colIdx)
          ?~ CellText txt
    else case Map.lookup txt imgs of
      --
      -- TODO : handle img link
      --
      Nothing ->
        sheet
          & cellValueAt (rowIdx, colIdx)
            ?~ CellText txt
      Just img ->
        sheet
          & #wsDrawing . _Just %~ \case
            Drawing xs ->
              Drawing $ newImg rowIdx colIdx (length xs) img : xs
  where
    txt = field ^. #fieldInput . #uniqueValue

newImg :: RowIndex -> ColumnIndex -> Int -> Rfc2397 -> Anchor FileInfo a
newImg (RowIndex rowIdx) (ColumnIndex colIdx) imgIdx rfc2397 =
  Anchor
    { anchAnchoring =
        TwoCellAnchor
          { tcaFrom = unqMarker (colIdx - 1, 0) (rowIdx - 1, 0),
            tcaTo = unqMarker (colIdx, 0) (rowIdx, 0),
            tcaEditAs = EditAsTwoCell
          },
      anchObject = obj,
      anchClientData = def
    }
  where
    obj =
      picture
        (DrawingElementId imgIdx)
        FileInfo
          { fiFilename = "img",
            fiContentType = decodeUtf8 $ rfc2397Mime rfc2397,
            fiContents = rfc2397Bytes rfc2397
          }

xlsxFile :: Unicode
xlsxFile = "delivery-calculator.xlsx"

xlsxMime :: Unicode
xlsxMime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
