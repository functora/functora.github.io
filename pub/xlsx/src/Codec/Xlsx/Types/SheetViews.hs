{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Xlsx.Types.SheetViews
  ( -- * Structured type to construct 'SheetViews'
    SheetView (..),
    Selection (..),
    Pane (..),
    SheetViewType (..),
    PaneType (..),
    PaneState (..),
  )
where

import GHC.Generics (Generic)

#ifdef USE_MICROLENS
import Lens.Micro.TH (makeLenses)
#else
import Control.Lens (makeLenses)
#endif
import Codec.Xlsx.Parser.Internal
import Codec.Xlsx.Types.Common
import Codec.Xlsx.Writer.Internal
import Control.DeepSeq (NFData)
import Data.Default
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Text.XML
import Text.XML.Cursor

{-------------------------------------------------------------------------------
  Main types
-------------------------------------------------------------------------------}

-- | Worksheet view
--
-- A single sheet view definition. When more than one sheet view is defined in
-- the file, it means that when opening the workbook, each sheet view
-- corresponds to a separate window within the spreadsheet application, where
-- each window is showing the particular sheet containing the same
-- workbookViewId value, the last sheetView definition is loaded, and the others
-- are discarded. When multiple windows are viewing the same sheet, multiple
-- sheetView elements (with corresponding workbookView entries) are saved.
--
-- TODO: The @pivotSelection@ and @extLst@ child elements are unsupported.
--
-- See Section 18.3.1.87 "sheetView (Worksheet View)" (p. 1880)
data SheetView = SheetView
  { -- | Index to the color value for row/column text headings and gridlines.
    -- This is an 'index color value' (ICV) rather than rgb value.
    sheetViewColorId :: Maybe Int,
    -- | Flag indicating that the consuming application should use the default
    -- grid lines color (system dependent). Overrides any color specified in
    -- colorId.
    sheetViewDefaultGridColor :: Maybe Bool,
    -- | Flag indicating whether the sheet is in 'right to left' display mode.
    -- When in this mode, Column A is on the far right, Column B ;is one column
    -- left of Column A, and so on. Also, information in cells is displayed in
    -- the Right to Left format.
    sheetViewRightToLeft :: Maybe Bool,
    -- | Flag indicating whether this sheet should display formulas.
    sheetViewShowFormulas :: Maybe Bool,
    -- | Flag indicating whether this sheet should display gridlines.
    sheetViewShowGridLines :: Maybe Bool,
    -- | Flag indicating whether the sheet has outline symbols visible. This
    -- flag shall always override SheetPr element's outlinePr child element
    -- whose attribute is named showOutlineSymbols when there is a conflict.
    sheetViewShowOutlineSymbols :: Maybe Bool,
    -- | Flag indicating whether the sheet should display row and column headings.
    sheetViewShowRowColHeaders :: Maybe Bool,
    -- | Show the ruler in Page Layout View.
    sheetViewShowRuler :: Maybe Bool,
    -- | Flag indicating whether page layout view shall display margins. False
    -- means do not display left, right, top (header), and bottom (footer)
    -- margins (even when there is data in the header or footer).
    sheetViewShowWhiteSpace :: Maybe Bool,
    -- | Flag indicating whether the window should show 0 (zero) in cells
    -- containing zero value. When false, cells with zero value appear blank
    -- instead of showing the number zero.
    sheetViewShowZeros :: Maybe Bool,
    -- | Flag indicating whether this sheet is selected. When only 1 sheet is
    -- selected and active, this value should be in synch with the activeTab
    -- value. In case of a conflict, the Start Part setting wins and sets the
    -- active sheet tab.
    --
    -- Multiple sheets can be selected, but only one sheet shall be active at
    -- one time.
    sheetViewTabSelected :: Maybe Bool,
    -- | Location of the top left visible cell Location of the top left visible
    -- cell in the bottom right pane (when in Left-to-Right mode).
    sheetViewTopLeftCell :: Maybe CellRef,
    -- | Indicates the view type.
    sheetViewType :: Maybe SheetViewType,
    -- | Flag indicating whether the panes in the window are locked due to
    -- workbook protection. This is an option when the workbook structure is
    -- protected.
    sheetViewWindowProtection :: Maybe Bool,
    -- | Zero-based index of this workbook view, pointing to a workbookView
    -- element in the bookViews collection.
    --
    -- NOTE: This attribute is required.
    sheetViewWorkbookViewId :: Int,
    -- | Window zoom magnification for current view representing percent values.
    -- This attribute is restricted to values ranging from 10 to 400. Horizontal &
    -- Vertical scale together.
    sheetViewZoomScale :: Maybe Int,
    -- | Zoom magnification to use when in normal view, representing percent
    -- values. This attribute is restricted to values ranging from 10 to 400.
    -- Horizontal & Vertical scale together.
    sheetViewZoomScaleNormal :: Maybe Int,
    -- | Zoom magnification to use when in page layout view, representing
    -- percent values. This attribute is restricted to values ranging from 10 to
    -- 400. Horizontal & Vertical scale together.
    sheetViewZoomScalePageLayoutView :: Maybe Int,
    -- | Zoom magnification to use when in page break preview, representing
    -- percent values. This attribute is restricted to values ranging from 10 to
    -- 400. Horizontal & Vertical scale together.
    sheetViewZoomScaleSheetLayoutView :: Maybe Int,
    -- | Worksheet view pane
    sheetViewPane :: Maybe Pane,
    -- | Worksheet view selection
    --
    -- Minimum of 0, maximum of 4 elements
    sheetViewSelection :: [Selection]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData SheetView

-- | Worksheet view selection.
--
-- Section 18.3.1.78 "selection (Selection)" (p. 1864)
data Selection = Selection
  { -- | Location of the active cell
    selectionActiveCell :: Maybe CellRef,
    -- | 0-based index of the range reference (in the array of references listed
    -- in sqref) containing the active cell. Only used when the selection in
    -- sqref is not contiguous. Therefore, this value needs to be aware of the
    -- order in which the range references are written in sqref.
    --
    -- When this value is out of range then activeCell can be used.
    selectionActiveCellId :: Maybe Int,
    -- | The pane to which this selection belongs.
    selectionPane :: Maybe PaneType,
    -- | Range of the selection. Can be non-contiguous set of ranges.
    selectionSqref :: Maybe SqRef
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData Selection

-- | Worksheet view pane
--
-- Section 18.3.1.66 "pane (View Pane)" (p. 1843)
data Pane = Pane
  { -- | The pane that is active.
    paneActivePane :: Maybe PaneType,
    -- | Indicates whether the pane has horizontal / vertical splits, and
    -- whether those splits are frozen.
    paneState :: Maybe PaneState,
    -- | Location of the top left visible cell in the bottom right pane (when in
    -- Left-To-Right mode).
    paneTopLeftCell :: Maybe CellRef,
    -- | Horizontal position of the split, in 1/20th of a point; 0 (zero) if
    -- none. If the pane is frozen, this value indicates the number of columns
    -- visible in the top pane.
    paneXSplit :: Maybe Double,
    -- | Vertical position of the split, in 1/20th of a point; 0 (zero) if none.
    -- If the pane is frozen, this value indicates the number of rows visible in
    -- the left pane.
    paneYSplit :: Maybe Double
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData Pane

{-------------------------------------------------------------------------------
  Enumerations
-------------------------------------------------------------------------------}

-- | View setting of the sheet
--
-- Section 18.18.69 "ST_SheetViewType (Sheet View Type)" (p. 2726)
data SheetViewType
  = -- | Normal view
    SheetViewTypeNormal
  | -- | Page break preview
    SheetViewTypePageBreakPreview
  | -- | Page layout view
    SheetViewTypePageLayout
  deriving (Eq, Ord, Show, Generic)

instance NFData SheetViewType

-- | Pane type
--
-- Section 18.18.52 "ST_Pane (Pane Types)" (p. 2710)
data PaneType
  = -- | Bottom left pane, when both vertical and horizontal splits are applied.
    --
    -- This value is also used when only a horizontal split has been applied,
    -- dividing the pane into upper and lower regions. In that case, this value
    -- specifies the bottom pane.
    PaneTypeBottomLeft
  | -- Bottom right pane, when both vertical and horizontal splits are applied.
    PaneTypeBottomRight
  | -- | Top left pane, when both vertical and horizontal splits are applied.
    --
    -- This value is also used when only a horizontal split has been applied,
    -- dividing the pane into upper and lower regions. In that case, this value
    -- specifies the top pane.
    --
    -- This value is also used when only a vertical split has been applied,
    -- dividing the pane into right and left regions. In that case, this value
    -- specifies the left pane
    PaneTypeTopLeft
  | -- | Top right pane, when both vertical and horizontal splits are applied.
    --
    -- This value is also used when only a vertical split has been applied,
    -- dividing the pane into right and left regions. In that case, this value
    -- specifies the right pane.
    PaneTypeTopRight
  deriving (Eq, Ord, Show, Generic)

instance NFData PaneType

-- | State of the sheet's pane.
--
-- Section 18.18.53 "ST_PaneState (Pane State)" (p. 2711)
data PaneState
  = -- | Panes are frozen, but were not split being frozen. In this state, when
    -- the panes are unfrozen again, a single pane results, with no split. In
    -- this state, the split bars are not adjustable.
    PaneStateFrozen
  | -- | Panes are frozen and were split before being frozen. In this state,
    -- when the panes are unfrozen again, the split remains, but is adjustable.
    PaneStateFrozenSplit
  | -- | Panes are split, but not frozen. In this state, the split bars are
    -- adjustable by the user.
    PaneStateSplit
  deriving (Eq, Ord, Show, Generic)

instance NFData PaneState

{-------------------------------------------------------------------------------
  Default instances
-------------------------------------------------------------------------------}

-- | NOTE: The 'Default' instance for 'SheetView' sets the required attribute
-- 'sheetViewWorkbookViewId' to @0@.
instance Default SheetView where
  def =
    SheetView
      { sheetViewColorId = Nothing,
        sheetViewDefaultGridColor = Nothing,
        sheetViewRightToLeft = Nothing,
        sheetViewShowFormulas = Nothing,
        sheetViewShowGridLines = Nothing,
        sheetViewShowOutlineSymbols = Nothing,
        sheetViewShowRowColHeaders = Nothing,
        sheetViewShowRuler = Nothing,
        sheetViewShowWhiteSpace = Nothing,
        sheetViewShowZeros = Nothing,
        sheetViewTabSelected = Nothing,
        sheetViewTopLeftCell = Nothing,
        sheetViewType = Nothing,
        sheetViewWindowProtection = Nothing,
        sheetViewWorkbookViewId = 0,
        sheetViewZoomScale = Nothing,
        sheetViewZoomScaleNormal = Nothing,
        sheetViewZoomScalePageLayoutView = Nothing,
        sheetViewZoomScaleSheetLayoutView = Nothing,
        sheetViewPane = Nothing,
        sheetViewSelection = []
      }

instance Default Selection where
  def =
    Selection
      { selectionActiveCell = Nothing,
        selectionActiveCellId = Nothing,
        selectionPane = Nothing,
        selectionSqref = Nothing
      }

instance Default Pane where
  def =
    Pane
      { paneActivePane = Nothing,
        paneState = Nothing,
        paneTopLeftCell = Nothing,
        paneXSplit = Nothing,
        paneYSplit = Nothing
      }

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | See @CT_SheetView@, p. 3913
instance ToElement SheetView where
  toElement nm SheetView {..} =
    Element
      { elementName = nm,
        elementNodes =
          map NodeElement . concat $
            [ map (toElement "pane") (maybeToList sheetViewPane),
              map (toElement "selection") sheetViewSelection
              -- TODO: pivotSelection
              -- TODO: extLst
            ],
        elementAttributes =
          Map.fromList . catMaybes $
            [ "windowProtection" .=? sheetViewWindowProtection,
              "showFormulas" .=? sheetViewShowFormulas,
              "showGridLines" .=? sheetViewShowGridLines,
              "showRowColHeaders" .=? sheetViewShowRowColHeaders,
              "showZeros" .=? sheetViewShowZeros,
              "rightToLeft" .=? sheetViewRightToLeft,
              "tabSelected" .=? sheetViewTabSelected,
              "showRuler" .=? sheetViewShowRuler,
              "showOutlineSymbols" .=? sheetViewShowOutlineSymbols,
              "defaultGridColor" .=? sheetViewDefaultGridColor,
              "showWhiteSpace" .=? sheetViewShowWhiteSpace,
              "view" .=? sheetViewType,
              "topLeftCell" .=? sheetViewTopLeftCell,
              "colorId" .=? sheetViewColorId,
              "zoomScale" .=? sheetViewZoomScale,
              "zoomScaleNormal" .=? sheetViewZoomScaleNormal,
              "zoomScaleSheetLayoutView" .=? sheetViewZoomScaleSheetLayoutView,
              "zoomScalePageLayoutView" .=? sheetViewZoomScalePageLayoutView,
              Just $ "workbookViewId" .= sheetViewWorkbookViewId
            ]
      }

-- | See @CT_Selection@, p. 3914
instance ToElement Selection where
  toElement nm Selection {..} =
    Element
      { elementName = nm,
        elementNodes = [],
        elementAttributes =
          Map.fromList . catMaybes $
            [ "pane" .=? selectionPane,
              "activeCell" .=? selectionActiveCell,
              "activeCellId" .=? selectionActiveCellId,
              "sqref" .=? selectionSqref
            ]
      }

-- | See @CT_Pane@, p. 3913
instance ToElement Pane where
  toElement nm Pane {..} =
    Element
      { elementName = nm,
        elementNodes = [],
        elementAttributes =
          Map.fromList . catMaybes $
            [ "xSplit" .=? paneXSplit,
              "ySplit" .=? paneYSplit,
              "topLeftCell" .=? paneTopLeftCell,
              "activePane" .=? paneActivePane,
              "state" .=? paneState
            ]
      }

-- | See @ST_SheetViewType@, p. 3913
instance ToAttrVal SheetViewType where
  toAttrVal SheetViewTypeNormal = "normal"
  toAttrVal SheetViewTypePageBreakPreview = "pageBreakPreview"
  toAttrVal SheetViewTypePageLayout = "pageLayout"

-- | See @ST_Pane@, p. 3914
instance ToAttrVal PaneType where
  toAttrVal PaneTypeBottomRight = "bottomRight"
  toAttrVal PaneTypeTopRight = "topRight"
  toAttrVal PaneTypeBottomLeft = "bottomLeft"
  toAttrVal PaneTypeTopLeft = "topLeft"

-- | See @ST_PaneState@, p. 3929
instance ToAttrVal PaneState where
  toAttrVal PaneStateSplit = "split"
  toAttrVal PaneStateFrozen = "frozen"
  toAttrVal PaneStateFrozenSplit = "frozenSplit"

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | See @CT_SheetView@, p. 3913
instance FromCursor SheetView where
  fromCursor cur = do
    sheetViewWindowProtection <- maybeAttribute "windowProtection" cur
    sheetViewShowFormulas <- maybeAttribute "showFormulas" cur
    sheetViewShowGridLines <- maybeAttribute "showGridLines" cur
    sheetViewShowRowColHeaders <- maybeAttribute "showRowColHeaders" cur
    sheetViewShowZeros <- maybeAttribute "showZeros" cur
    sheetViewRightToLeft <- maybeAttribute "rightToLeft" cur
    sheetViewTabSelected <- maybeAttribute "tabSelected" cur
    sheetViewShowRuler <- maybeAttribute "showRuler" cur
    sheetViewShowOutlineSymbols <- maybeAttribute "showOutlineSymbols" cur
    sheetViewDefaultGridColor <- maybeAttribute "defaultGridColor" cur
    sheetViewShowWhiteSpace <- maybeAttribute "showWhiteSpace" cur
    sheetViewType <- maybeAttribute "view" cur
    sheetViewTopLeftCell <- maybeAttribute "topLeftCell" cur
    sheetViewColorId <- maybeAttribute "colorId" cur
    sheetViewZoomScale <- maybeAttribute "zoomScale" cur
    sheetViewZoomScaleNormal <- maybeAttribute "zoomScaleNormal" cur
    sheetViewZoomScaleSheetLayoutView <-
      maybeAttribute "zoomScaleSheetLayoutView" cur
    sheetViewZoomScalePageLayoutView <-
      maybeAttribute "zoomScalePageLayoutView" cur
    sheetViewWorkbookViewId <- fromAttribute "workbookViewId" cur
    let sheetViewPane = listToMaybe $ cur $/ element (n_ "pane") >=> fromCursor
        sheetViewSelection = cur $/ element (n_ "selection") >=> fromCursor
    return SheetView {..}

instance FromXenoNode SheetView where
  fromXenoNode root = parseAttributes root $ do
    sheetViewWindowProtection <- maybeAttr "windowProtection"
    sheetViewShowFormulas <- maybeAttr "showFormulas"
    sheetViewShowGridLines <- maybeAttr "showGridLines"
    sheetViewShowRowColHeaders <- maybeAttr "showRowColHeaders"
    sheetViewShowZeros <- maybeAttr "showZeros"
    sheetViewRightToLeft <- maybeAttr "rightToLeft"
    sheetViewTabSelected <- maybeAttr "tabSelected"
    sheetViewShowRuler <- maybeAttr "showRuler"
    sheetViewShowOutlineSymbols <- maybeAttr "showOutlineSymbols"
    sheetViewDefaultGridColor <- maybeAttr "defaultGridColor"
    sheetViewShowWhiteSpace <- maybeAttr "showWhiteSpace"
    sheetViewType <- maybeAttr "view"
    sheetViewTopLeftCell <- maybeAttr "topLeftCell"
    sheetViewColorId <- maybeAttr "colorId"
    sheetViewZoomScale <- maybeAttr "zoomScale"
    sheetViewZoomScaleNormal <- maybeAttr "zoomScaleNormal"
    sheetViewZoomScaleSheetLayoutView <- maybeAttr "zoomScaleSheetLayoutView"
    sheetViewZoomScalePageLayoutView <- maybeAttr "zoomScalePageLayoutView"
    sheetViewWorkbookViewId <- fromAttr "workbookViewId"
    (sheetViewPane, sheetViewSelection) <-
      toAttrParser . collectChildren root $
        (,) <$> maybeFromChild "pane" <*> fromChildList "selection"
    return SheetView {..}

-- | See @CT_Pane@, p. 3913
instance FromCursor Pane where
  fromCursor cur = do
    paneXSplit <- maybeAttribute "xSplit" cur
    paneYSplit <- maybeAttribute "ySplit" cur
    paneTopLeftCell <- maybeAttribute "topLeftCell" cur
    paneActivePane <- maybeAttribute "activePane" cur
    paneState <- maybeAttribute "state" cur
    return Pane {..}

instance FromXenoNode Pane where
  fromXenoNode root =
    parseAttributes root $ do
      paneXSplit <- maybeAttr "xSplit"
      paneYSplit <- maybeAttr "ySplit"
      paneTopLeftCell <- maybeAttr "topLeftCell"
      paneActivePane <- maybeAttr "activePane"
      paneState <- maybeAttr "state"
      return Pane {..}

-- | See @CT_Selection@, p. 3914
instance FromCursor Selection where
  fromCursor cur = do
    selectionPane <- maybeAttribute "pane" cur
    selectionActiveCell <- maybeAttribute "activeCell" cur
    selectionActiveCellId <- maybeAttribute "activeCellId" cur
    selectionSqref <- maybeAttribute "sqref" cur
    return Selection {..}

instance FromXenoNode Selection where
  fromXenoNode root =
    parseAttributes root $ do
      selectionPane <- maybeAttr "pane"
      selectionActiveCell <- maybeAttr "activeCell"
      selectionActiveCellId <- maybeAttr "activeCellId"
      selectionSqref <- maybeAttr "sqref"
      return Selection {..}

-- | See @ST_SheetViewType@, p. 3913
instance FromAttrVal SheetViewType where
  fromAttrVal "normal" = readSuccess SheetViewTypeNormal
  fromAttrVal "pageBreakPreview" = readSuccess SheetViewTypePageBreakPreview
  fromAttrVal "pageLayout" = readSuccess SheetViewTypePageLayout
  fromAttrVal t = invalidText "SheetViewType" t

instance FromAttrBs SheetViewType where
  fromAttrBs "normal" = return SheetViewTypeNormal
  fromAttrBs "pageBreakPreview" = return SheetViewTypePageBreakPreview
  fromAttrBs "pageLayout" = return SheetViewTypePageLayout
  fromAttrBs x = unexpectedAttrBs "SheetViewType" x

-- | See @ST_Pane@, p. 3914
instance FromAttrVal PaneType where
  fromAttrVal "bottomRight" = readSuccess PaneTypeBottomRight
  fromAttrVal "topRight" = readSuccess PaneTypeTopRight
  fromAttrVal "bottomLeft" = readSuccess PaneTypeBottomLeft
  fromAttrVal "topLeft" = readSuccess PaneTypeTopLeft
  fromAttrVal t = invalidText "PaneType" t

instance FromAttrBs PaneType where
  fromAttrBs "bottomRight" = return PaneTypeBottomRight
  fromAttrBs "topRight" = return PaneTypeTopRight
  fromAttrBs "bottomLeft" = return PaneTypeBottomLeft
  fromAttrBs "topLeft" = return PaneTypeTopLeft
  fromAttrBs x = unexpectedAttrBs "PaneType" x

-- | See @ST_PaneState@, p. 3929
instance FromAttrVal PaneState where
  fromAttrVal "split" = readSuccess PaneStateSplit
  fromAttrVal "frozen" = readSuccess PaneStateFrozen
  fromAttrVal "frozenSplit" = readSuccess PaneStateFrozenSplit
  fromAttrVal t = invalidText "PaneState" t

instance FromAttrBs PaneState where
  fromAttrBs "split" = return PaneStateSplit
  fromAttrBs "frozen" = return PaneStateFrozen
  fromAttrBs "frozenSplit" = return PaneStateFrozenSplit
  fromAttrBs x = unexpectedAttrBs "PaneState" x
