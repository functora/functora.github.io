module App.Xlsx
  ( newXlsx,
    xlsxFile,
    xlsxMime,
  )
where

import qualified App.I18n as I18n
import App.Types
import Codec.Xlsx
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Time.Format as TF
import Functora.Miso.Prelude
import Lens.Micro ((?~))

newXlsx :: St Unique -> Map Unicode Rfc2397 -> BL.ByteString
newXlsx st imgs = xlsx
  where
    xlsx =
      fromXlsx 0
        $ def
        & atSheet "Cryptogram"
        ?~ sheet
    sheet =
      def
        & #wsDrawing
        .~ Just (Drawing mempty)
        & #wsRowPropertiesMap
        .~ newRowProps rows
        & #wsColumnsProperties
        .~ newColProps (fmap snd rows)
        & addHeader
        & flip (foldl $ addRow imgs) rows
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
          { cpMin = mapCol colIdx,
            cpMax = mapCol colIdx,
            cpWidth = Just 45,
            cpStyle = Nothing,
            cpHidden = False,
            cpCollapsed = False,
            cpBestFit = False
          }

addHeader :: Worksheet -> Worksheet
addHeader sheet =
  foldl
    ( \acc (colIdx, colVal) ->
        acc
          & cellValueAt (1, colIdx)
          ?~ CellText (from @Unicode @Text colVal)
    )
    sheet
    $ zip [1 ..] I18n.xlsxHeaderRu

addRow ::
  Map Unicode Rfc2397 ->
  Worksheet ->
  (RowIndex, [Field DynamicField Unique]) ->
  Worksheet
addRow imgs sheet (rowIdx, rowVal) =
  foldl
    ( \acc (colIdx, colVal) ->
        addCol imgs acc rowIdx (mapCol colIdx) colVal
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

xlsxFile :: (MonadIO m) => m Unicode
xlsxFile = do
  ct <- getCurrentTime
  pure
    . from @String @Unicode
    $ "cryptogram-"
    <> TF.formatTime TF.defaultTimeLocale "%Y%m%d%H%M%S" ct
    <> ".xlsx"

xlsxMime :: Unicode
xlsxMime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

mapCol :: (Integral n) => n -> n
mapCol = \case
  1 -> 2
  2 -> 3
  3 -> 5
  4 -> 6
  5 -> 10
  n -> n + 10
