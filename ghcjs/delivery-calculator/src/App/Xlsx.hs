module App.Xlsx (newXlsx) where

import Codec.Xlsx
import qualified Data.ByteString.Lazy as BL
import Functora.Miso.Prelude
import Lens.Micro ((?~))

newXlsx :: ByteString
newXlsx =
  from @BL.ByteString @ByteString
    . fromXlsx 0
    $ def
    & atSheet "List1"
    ?~ sheet
  where
    sheet =
      def
        & cellValueAt (1, 2)
        ?~ CellDouble 42.0
          & cellValueAt (3, 2)
        ?~ CellText "foo"
