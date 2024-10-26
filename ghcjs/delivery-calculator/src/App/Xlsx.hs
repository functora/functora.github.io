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
import Functora.Miso.Prelude
import Lens.Micro ((?~), (^..))

newXlsx :: St Unique -> Map Unicode Rfc2397 -> BL.ByteString
newXlsx st imgs = xlsx
  where
    xlsx =
      fromXlsx 0
        $ def
        & atSheet "Delivery Calculator"
        ?~ sheet
    sheet =
      def
        & #wsDrawing
        .~ Just (Drawing mempty)
        & #wsRowPropertiesMap
        .~ newRowProps rows
        & #wsColumnsProperties
        .~ newColProps (fmap snd rows)
        & addHeader st
        & flip (foldl $ addRow imgs) rows
        & addFooter st (RowIndex (length rows) + 2)
    rows =
      zip [2 ..]
        $ fmap
          (^.. #assetFieldPairs . each . #fieldPairValue)
          (st ^. #stAssets)

newRowProps ::
  [(RowIndex, [Field DynamicField Unique])] ->
  Map RowIndex RowProperties
newRowProps =
  Map.fromList
    . catMaybes
    . fmap
      ( \(rowIdx, rowVal) ->
          if any (\x -> x ^. #fieldType == FieldTypeImage) rowVal
            then Just (rowIdx, def & #rowHeight ?~ CustomHeight 180)
            else Nothing
      )

newColProps :: [[Field DynamicField Unique]] -> [ColumnsProperties]
newColProps rows = nubOrd $ do
  row <- rows
  (colIdx, colVal) <- zip [1 ..] row
  if colVal ^. #fieldType /= FieldTypeImage
    then mempty
    else
      pure
        ColumnsProperties
          { cpMin = colIdx,
            cpMax = colIdx,
            cpWidth = Just 45,
            cpStyle = Nothing,
            cpHidden = False,
            cpCollapsed = False,
            cpBestFit = False
          }

addHeader :: St Unique -> Worksheet -> Worksheet
addHeader st sheet =
  case sortOn length headers of
    [] -> sheet
    (rowVal : _) ->
      foldl
        ( \acc (colIdx, colVal) ->
            acc
              & cellValueAt (1, colIdx)
              ?~ CellText (from @Unicode @Text colVal)
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

addFooter :: St Unique -> RowIndex -> Worksheet -> Worksheet
addFooter st rowOffset sheet =
  foldl
    ( \acc (rowIdx, rowVal) ->
        acc
          & cellValueAt (rowOffset + rowIdx, 1)
          ?~ CellText
            ( from @Unicode @Text
                $ rowVal
                ^. #fieldPairKey
                . #fieldInput
                . #runIdentity
            )
            & cellValueAt (rowOffset + rowIdx, 2)
          ?~ CellText
            ( from @Unicode @Text
                $ rowVal
                ^. #fieldPairValue
                . #fieldInput
                . #runIdentity
            )
    )
    sheet
    . zip [1 ..]
    $ newTotal st

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
        ?~ CellText (from @Unicode @Text txt)
    else case Map.lookup txt imgs of
      --
      -- TODO : handle img link
      --
      Nothing ->
        sheet
          & cellValueAt (rowIdx, colIdx)
          ?~ CellText (from @Unicode @Text txt)
      Just img ->
        sheet
          & #wsDrawing
          . _Just
          %~ \case
            Drawing xs ->
              Drawing $ newImg rowIdx colIdx (length xs + 1) img : xs
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
    mime = decodeUtf8 $ rfc2397Mime rfc2397
    ext =
      from @Text @String
        . maybe mempty ("." <>)
        . safeHead
        . reverse
        $ splitWhen (== '/') mime
    obj =
      picture
        (DrawingElementId imgIdx)
        FileInfo
          { fiFilename = "image" <> inspect imgIdx <> ext,
            fiContentType = mime,
            fiContents = rfc2397Bytes rfc2397
          }

xlsxFile :: St Unique -> Unicode
xlsxFile st =
  "delivery-calculator-"
    <> (st ^. #stOrderId . #fieldOutput)
    <> ".xlsx"

xlsxMime :: Unicode
xlsxMime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
