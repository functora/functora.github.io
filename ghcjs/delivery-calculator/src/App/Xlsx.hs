{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module App.Xlsx
  ( newXlsx,
    xlsxFile,
    xlsxMime,
  )
where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Functora.Miso.Prelude
import Lens.Micro hiding (each, to)
import qualified Prelude

newXlsx :: Map Unicode Rfc2397 -> BL.ByteString
newXlsx imgs = xlsx
  where
    xlsx =
      fromXlsx 0
        $ def
        & atSheet "List1" ?~ sheet
    sheet =
      def
        & cellValueAt (1, 2) ?~ CellDouble 42.0
        & cellValueAt (3, 2) ?~ CellText "foo"
        & #wsDrawing ?~ drawing (Prelude.head $ Map.elems imgs)

drawing :: Rfc2397 -> Drawing
drawing rfc2397 = Drawing [anchor1]
  where
    anchor1 =
      Anchor
        { anchAnchoring =
            TwoCellAnchor
              { tcaFrom = unqMarker (1, 0) (1, 0),
                tcaTo = unqMarker (5, 0) (13, 0),
                tcaEditAs = EditAsTwoCell
              },
          anchObject = obj,
          anchClientData = def
        }
    obj =
      picture
        (DrawingElementId 0)
        FileInfo
          { fiFilename = "img",
            fiContentType = decodeUtf8 $ rfc2397Mime rfc2397,
            fiContents = rfc2397Bytes rfc2397
          }

xlsxFile :: Unicode
xlsxFile = "delivery-calculator.xlsx"

xlsxMime :: Unicode
xlsxMime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
