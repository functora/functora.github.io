module App.Xlsx (newXlsx) where

import Functora.Miso.Prelude
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)

newXlsx :: ByteString
newXlsx =
  encodeUtf8 $ showSpreadsheet workbook
  where
    cells1 :: [[Cell]]
    cells1 =
      [ [string "Quantity \174", string "Multiplier \8480", string "Product"],
        [number 1, number 0.9, formula "=RC[-2]*RC[-1]"],
        [number 10, number 1.1, formula "=RC[-2]*RC[-1]"],
        [number 12, number 0.2, formula "=RC[-2]*RC[-1]"]
      ]
    worksheet1 = mkWorksheet (Name "Quantity Product Sheet") (tableFromCells cells1)
    cells2 :: [[Cell]]
    cells2 =
      [ [string "Quantity1", string "Quantity2", string "Sum"],
        [number 1, number 100, formula "=RC[-2]+RC[-1]"],
        [number 10, number 201, formula "=RC[-2]+RC[-1]"],
        [number 12, number 45, formula "=RC[-2]+RC[-1]"]
      ]
    worksheet2 = mkWorksheet (Name "Quantity Sum Sheet") (tableFromCells cells2)
    workbook = mkWorkbook [worksheet1, worksheet2]
