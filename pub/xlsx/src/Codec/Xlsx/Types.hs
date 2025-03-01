{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Codec.Xlsx.Types
  ( -- * The main types
    Xlsx (..),
    Styles (..),
    DefinedNames (..),
    ColumnsProperties (..),
    PageSetup (..),
    Worksheet (..),
    SheetState (..),
    CellMap,
    CellValue (..),
    CellFormula (..),
    FormulaExpression (..),
    Cell.SharedFormulaIndex (..),
    Cell.SharedFormulaOptions (..),
    Cell (..),
    RowHeight (..),
    RowProperties (..),

    -- * Lenses

    -- ** Row properties
    rowHeightLens,
    _CustomHeight,
    _AutomaticHeight,

    -- * Style helpers
    emptyStyles,
    renderStyleSheet,
    parseStyleSheet,

    -- * Misc
    simpleCellFormula,
    sharedFormulaByIndex,
    def,
    toRows,
    fromRows,
    module X,
  )
where

import Codec.Xlsx.Parser.Internal
import Codec.Xlsx.Types.AutoFilter as X
import Codec.Xlsx.Types.Cell as Cell
import Codec.Xlsx.Types.Comment as X
import Codec.Xlsx.Types.Common as X
import Codec.Xlsx.Types.ConditionalFormatting as X
import Codec.Xlsx.Types.DataValidation as X
import Codec.Xlsx.Types.Drawing as X
import Codec.Xlsx.Types.Drawing.Chart as X
import Codec.Xlsx.Types.Drawing.Common as X
import Codec.Xlsx.Types.PageSetup as X
import Codec.Xlsx.Types.PivotTable as X
import Codec.Xlsx.Types.Protection as X
import Codec.Xlsx.Types.RichText as X
import Codec.Xlsx.Types.SheetViews as X
import Codec.Xlsx.Types.StyleSheet as X
import Codec.Xlsx.Types.Table as X
import Codec.Xlsx.Types.Variant as X
import Codec.Xlsx.Writer.Internal
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, toException)
import qualified Data.ByteString.Lazy as L
import Data.Default
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.XML (parseLBS, renderLBS)
import Text.XML.Cursor
#ifdef USE_MICROLENS
import Lens.Micro
#else
import Control.Lens (lens, Lens')
#endif

-- | Height of a row in points (1/72in)
data RowHeight
  = -- | Row height is set by the user
    CustomHeight !Double
  | -- | Row height is set automatically by the program
    AutomaticHeight !Double
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData RowHeight

-- Since micro-lens denies the existence of prisms,
-- I pasted the splice that's generated from makePrisms,
-- then I copied over the definitions from Control.Lens for the prism
-- function as well.
type Prism s t a b =
  forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

_CustomHeight :: Prism' RowHeight Double
_CustomHeight =
  (prism (\x1_a4xgd -> CustomHeight x1_a4xgd))
    ( \x_a4xge ->
        case x_a4xge of
          CustomHeight y1_a4xgf -> Right y1_a4xgf
          _ -> Left x_a4xge
    )
{-# INLINE _CustomHeight #-}

_AutomaticHeight :: Prism' RowHeight Double
_AutomaticHeight =
  (prism (\x1_a4xgg -> AutomaticHeight x1_a4xgg))
    ( \x_a4xgh ->
        case x_a4xgh of
          AutomaticHeight y1_a4xgi -> Right y1_a4xgi
          _ -> Left x_a4xgh
    )
{-# INLINE _AutomaticHeight #-}

-- | Properties of a row. See 18.3.1.73 "row (Row)" for more details
data RowProperties = RowProps
  { -- | Row height in points
    rowHeight :: Maybe RowHeight,
    -- | Style to be applied to row
    rowStyle :: Maybe Int,
    -- | Whether row is visible or not
    rowHidden :: Bool
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData RowProperties

rowHeightLens :: Lens' RowProperties (Maybe RowHeight)
rowHeightLens = lens rowHeight $ \x y -> x {rowHeight = y}

instance Default RowProperties where
  def =
    RowProps
      { rowHeight = Nothing,
        rowStyle = Nothing,
        rowHidden = False
      }

-- | Column range (from cwMin to cwMax) properties
data ColumnsProperties = ColumnsProperties
  { -- | First column affected by this 'ColumnWidth' record.
    cpMin :: Int,
    -- | Last column affected by this 'ColumnWidth' record.
    cpMax :: Int,
    -- | Column width measured as the number of characters of the
    -- maximum digit width of the numbers 0, 1, 2, ..., 9 as rendered in
    -- the normal style's font.
    --
    -- See longer description in Section 18.3.1.13 "col (Column Width &
    -- Formatting)" (p. 1605)
    cpWidth :: Maybe Double,
    -- | Default style for the affected column(s). Affects cells not yet
    -- allocated in the column(s).  In other words, this style applies
    -- to new columns.
    cpStyle :: Maybe Int,
    -- | Flag indicating if the affected column(s) are hidden on this
    -- worksheet.
    cpHidden :: Bool,
    -- | Flag indicating if the outlining of the affected column(s) is
    -- in the collapsed state.
    cpCollapsed :: Bool,
    -- | Flag indicating if the specified column(s) is set to 'best
    -- fit'.
    cpBestFit :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData ColumnsProperties

instance FromCursor ColumnsProperties where
  fromCursor c = do
    cpMin <- fromAttribute "min" c
    cpMax <- fromAttribute "max" c
    cpWidth <- maybeAttribute "width" c
    cpStyle <- maybeAttribute "style" c
    cpHidden <- fromAttributeDef "hidden" False c
    cpCollapsed <- fromAttributeDef "collapsed" False c
    cpBestFit <- fromAttributeDef "bestFit" False c
    return ColumnsProperties {..}

instance FromXenoNode ColumnsProperties where
  fromXenoNode root = parseAttributes root $ do
    cpMin <- fromAttr "min"
    cpMax <- fromAttr "max"
    cpWidth <- maybeAttr "width"
    cpStyle <- maybeAttr "style"
    cpHidden <- fromAttrDef "hidden" False
    cpCollapsed <- fromAttrDef "collapsed" False
    cpBestFit <- fromAttrDef "bestFit" False
    return ColumnsProperties {..}

-- | Sheet visibility state
-- cf. Ecma Office Open XML Part 1:
-- 18.18.68 ST_SheetState (Sheet Visibility Types)
-- * "visible"
--     Indicates the sheet is visible (default)
-- * "hidden"
--     Indicates the workbook window is hidden, but can be shown by the user via the user interface.
-- * "veryHidden"
--     Indicates the sheet is hidden and cannot be shown in the user interface (UI). This state is only available programmatically.
data SheetState
  = -- | state="visible"
    Visible
  | -- | state="hidden"
    Hidden
  | -- | state="veryHidden"
    VeryHidden
  deriving (Eq, Show, Generic)

instance NFData SheetState

instance Default SheetState where
  def = Visible

instance FromAttrVal SheetState where
  fromAttrVal "visible" = readSuccess Visible
  fromAttrVal "hidden" = readSuccess Hidden
  fromAttrVal "veryHidden" = readSuccess VeryHidden
  fromAttrVal t = invalidText "SheetState" t

instance FromAttrBs SheetState where
  fromAttrBs "visible" = return Visible
  fromAttrBs "hidden" = return Hidden
  fromAttrBs "veryHidden" = return VeryHidden
  fromAttrBs t = unexpectedAttrBs "SheetState" t

instance ToAttrVal SheetState where
  toAttrVal Visible = "visible"
  toAttrVal Hidden = "hidden"
  toAttrVal VeryHidden = "veryHidden"

-- | Xlsx worksheet
data Worksheet = Worksheet
  { -- | column widths
    wsColumnsProperties :: [ColumnsProperties],
    -- | custom row properties (height, style) map
    wsRowPropertiesMap :: Map RowIndex RowProperties,
    -- | data mapped by (row, column) pairs
    wsCells :: CellMap,
    -- | SpreadsheetML Drawing
    wsDrawing :: Maybe Drawing,
    -- | list of cell merges
    wsMerges :: [Range],
    wsSheetViews :: Maybe [SheetView],
    wsPageSetup :: Maybe PageSetup,
    wsConditionalFormattings :: Map SqRef ConditionalFormatting,
    wsDataValidations :: Map SqRef DataValidation,
    wsPivotTables :: [PivotTable],
    wsAutoFilter :: Maybe AutoFilter,
    wsTables :: [Table],
    wsProtection :: Maybe SheetProtection,
    wsSharedFormulas :: Map SharedFormulaIndex SharedFormulaOptions,
    wsState :: SheetState
  }
  deriving (Eq, Show, Generic)

instance NFData Worksheet

instance Default Worksheet where
  def =
    Worksheet
      { wsColumnsProperties = [],
        wsRowPropertiesMap = M.empty,
        wsCells = M.empty,
        wsDrawing = Nothing,
        wsMerges = [],
        wsSheetViews = Nothing,
        wsPageSetup = Nothing,
        wsConditionalFormattings = M.empty,
        wsDataValidations = M.empty,
        wsPivotTables = [],
        wsAutoFilter = Nothing,
        wsTables = [],
        wsProtection = Nothing,
        wsSharedFormulas = M.empty,
        wsState = def
      }

-- | Raw worksheet styles, for structured implementation see 'StyleSheet'
-- and functions in "Codec.Xlsx.Types.StyleSheet"
newtype Styles = Styles {unStyles :: L.ByteString}
  deriving (Eq, Show, Generic)

instance NFData Styles

-- | Structured representation of Xlsx file (currently a subset of its contents)
data Xlsx = Xlsx
  { xlSheets :: [(Text, Worksheet)],
    xlStyles :: Styles,
    xlDefinedNames :: DefinedNames,
    xlCustomProperties :: Map Text Variant,
    -- | date base to use when converting serial value (i.e. 'CellDouble d')
    -- into date-time. Default value is 'DateBase1900'
    --
    -- See also 18.17.4.1 "Date Conversion for Serial Date-Times" (p. 2067)
    xlDateBase :: DateBase
  }
  deriving (Eq, Show, Generic)

instance NFData Xlsx

-- | Defined names
--
-- Each defined name consists of a name, an optional local sheet ID, and a value.
--
-- This element defines the collection of defined names for this workbook.
-- Defined names are descriptive names to represent cells, ranges of cells,
-- formulas, or constant values. Defined names can be used to represent a range
-- on any worksheet.
--
-- Excel also defines a number of reserved names with a special interpretation:
--
-- * @_xlnm.Print_Area@ specifies the workbook's print area.
--   Example value: @SheetName!$A:$A,SheetName!$1:$4@
-- * @_xlnm.Print_Titles@ specifies the row(s) or column(s) to repeat
--   at the top of each printed page.
-- * @_xlnm.Sheet_Title@:refers to a sheet title.
--
-- and others. See Section 18.2.6, "definedNames (Defined Names)" (p. 1728) of
-- the spec (second edition).
--
-- NOTE: Right now this is only a minimal implementation of defined names.
newtype DefinedNames = DefinedNames [(Text, Maybe Text, Text)]
  deriving (Eq, Show, Generic)

instance NFData DefinedNames

instance Default Xlsx where
  def = Xlsx [] emptyStyles def M.empty DateBase1900

instance Default DefinedNames where
  def = DefinedNames []

emptyStyles :: Styles
emptyStyles =
  Styles
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"></styleSheet>"

-- | Render 'StyleSheet'
--
-- This is used to render a structured 'StyleSheet' into a raw XML 'Styles'
-- document. Actually /replacing/ 'Styles' with 'StyleSheet' would mean we
-- would need to write a /parser/ for 'StyleSheet' as well (and would moreover
-- require that we support the full style sheet specification, which is still
-- quite a bit of work).
renderStyleSheet :: StyleSheet -> Styles
renderStyleSheet = Styles . renderLBS def . toDocument

-- | Parse 'StyleSheet'
--
-- This is used to parse raw 'Styles' into structured 'StyleSheet'
-- currently not all of the style sheet specification is supported
-- so parser (and the data model) is to be completed
parseStyleSheet :: Styles -> Either SomeException StyleSheet
parseStyleSheet (Styles bs) = parseLBS def bs >>= parseDoc
  where
    parseDoc doc = case fromCursor (fromDocument doc) of
      [stylesheet] -> Right stylesheet
      _ -> Left . toException $ ParseException "Could not parse style sheets"

-- | converts cells mapped by (row, column) into rows which contain
-- row index and cells as pairs of column indices and cell values
toRows :: CellMap -> [(RowIndex, [(ColumnIndex, Cell)])]
toRows cells =
  map extractRow $ groupBy ((==) `on` (fst . fst)) $ M.toList cells
  where
    extractRow row@(((x, _), _) : _) =
      (x, map (\((_, y), v) -> (y, v)) row)
    extractRow _ = error "invalid CellMap row"

-- | reverse to 'toRows'
fromRows :: [(RowIndex, [(ColumnIndex, Cell)])] -> CellMap
fromRows rows = M.fromList $ concatMap mapRow rows
  where
    mapRow (r, cells) = map (\(c, v) -> ((r, c), v)) cells

instance ToElement ColumnsProperties where
  toElement nm ColumnsProperties {..} = leafElement nm attrs
    where
      attrs =
        ["min" .= cpMin, "max" .= cpMax]
          ++ catMaybes
            [ "style" .=? (justNonDef 0 =<< cpStyle),
              "width" .=? cpWidth,
              "customWidth" .=? justTrue (isJust cpWidth),
              "hidden" .=? justTrue cpHidden,
              "collapsed" .=? justTrue cpCollapsed,
              "bestFit" .=? justTrue cpBestFit
            ]
