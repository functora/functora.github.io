{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Higher level interface for creating styled worksheets
module Codec.Xlsx.Formatted
  ( FormattedCell (..),
    Formatted (..),
    Format (..),
    formatted,
    formatWorkbook,
    toFormattedCells,
    CondFormatted (..),
    conditionallyFormatted,
  )
where

#ifdef USE_MICROLENS
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro.GHC ()
#else
import Control.Lens
#endif
import Codec.Xlsx.Types
import Control.Monad (forM, guard)
import Control.Monad.State hiding (forM_, mapM)
import Data.Default
import Data.Foldable (asum, forM_)
import Data.Function (on)
import Data.Generics.Labels
import Data.List (foldl', groupBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Traversable (mapM)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Safe (fromJustNote, headNote)
import Prelude hiding (mapM)

{-------------------------------------------------------------------------------
  Internal: formatting state
-------------------------------------------------------------------------------}

data FormattingState = FormattingState
  { formattingBorders :: Map Border Int,
    formattingCellXfs :: Map CellXf Int,
    formattingFills :: Map Fill Int,
    formattingFonts :: Map Font Int,
    formattingNumFmts :: Map Text Int,
    -- | In reverse order
    formattingMerges :: [Range]
  }
  deriving stock (Generic)

stateFromStyleSheet :: StyleSheet -> FormattingState
stateFromStyleSheet StyleSheet {..} =
  FormattingState
    { formattingBorders = fromValueList styleSheetBorders,
      formattingCellXfs = fromValueList styleSheetCellXfs,
      formattingFills = fromValueList styleSheetFills,
      formattingFonts = fromValueList styleSheetFonts,
      formattingNumFmts = M.fromList . map swap $ M.toList styleSheetNumFmts,
      formattingMerges = []
    }

fromValueList :: (Ord a) => [a] -> Map a Int
fromValueList = M.fromList . (`zip` [0 ..])

toValueList :: Map a Int -> [a]
toValueList = map snd . sortBy (comparing fst) . map swap . M.toList

updateStyleSheetFromState :: StyleSheet -> FormattingState -> StyleSheet
updateStyleSheetFromState sSheet FormattingState {..} =
  sSheet
    { styleSheetBorders = toValueList formattingBorders,
      styleSheetCellXfs = toValueList formattingCellXfs,
      styleSheetFills = toValueList formattingFills,
      styleSheetFonts = toValueList formattingFonts,
      styleSheetNumFmts = M.fromList . map swap $ M.toList formattingNumFmts
    }

getId ::
  (Ord a) => Lens' FormattingState (Map a Int) -> a -> State FormattingState Int
getId = getId' 0

getId' ::
  (Ord a) =>
  Int ->
  Lens' FormattingState (Map a Int) ->
  a ->
  State FormattingState Int
getId' k f v = do
  aMap <- use f
  case M.lookup v aMap of
    Just anId -> return anId
    Nothing -> do
      let anId = k + M.size aMap
      f %= M.insert v anId
      return anId

{-------------------------------------------------------------------------------
  Unwrapped cell conditional formatting
-------------------------------------------------------------------------------}

data FormattedCondFmt = FormattedCondFmt
  { condfmtCondition :: Condition,
    condfmtDxf :: Dxf,
    condfmtPriority :: Int,
    condfmtStopIfTrue :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

{-------------------------------------------------------------------------------
  Cell with formatting
-------------------------------------------------------------------------------}

-- | Formatting options used to format cells
--
-- TODOs:
--
-- * Add a number format ('cellXfApplyNumberFormat', 'cellXfNumFmtId')
-- * Add references to the named style sheets ('cellXfId')
data Format = Format
  { formatAlignment :: Maybe Alignment,
    formatBorder :: Maybe Border,
    formatFill :: Maybe Fill,
    formatFont :: Maybe Font,
    formatNumberFormat :: Maybe NumberFormat,
    formatProtection :: Maybe Protection,
    formatPivotButton :: Maybe Bool,
    formatQuotePrefix :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

-- | Cell with formatting. 'cellStyle' property of 'formattedCell' is ignored
--
-- See 'formatted' for more details.
data FormattedCell = FormattedCell
  { formattedCell :: Cell,
    formattedFormat :: Format,
    formattedColSpan :: Int,
    formattedRowSpan :: Int
  }
  deriving (Eq, Show, Generic)

{-------------------------------------------------------------------------------
  Default instances
-------------------------------------------------------------------------------}

instance Default FormattedCell where
  def =
    FormattedCell
      { formattedCell = def,
        formattedFormat = def,
        formattedColSpan = 1,
        formattedRowSpan = 1
      }

instance Default Format where
  def =
    Format
      { formatAlignment = Nothing,
        formatBorder = Nothing,
        formatFill = Nothing,
        formatFont = Nothing,
        formatNumberFormat = Nothing,
        formatProtection = Nothing,
        formatPivotButton = Nothing,
        formatQuotePrefix = Nothing
      }

instance Default FormattedCondFmt where
  def = FormattedCondFmt ContainsBlanks def topCfPriority Nothing

{-------------------------------------------------------------------------------
  Client-facing API
-------------------------------------------------------------------------------}

-- | Result of formatting
--
-- See 'formatted'
data Formatted = Formatted
  { -- | The final 'CellMap'; see '_wsCells'
    formattedCellMap :: CellMap,
    -- | The final stylesheet; see '_xlStyles' (and 'renderStyleSheet')
    formattedStyleSheet :: StyleSheet,
    -- | The final list of cell merges; see '_wsMerges'
    formattedMerges :: [Range]
  }
  deriving (Eq, Show, Generic)

-- | Higher level API for creating formatted documents
--
-- Creating formatted Excel spreadsheets using the 'Cell' datatype directly,
-- even with the support for the 'StyleSheet' datatype, is fairly painful.
-- This has a number of causes:
--
-- * The 'Cell' datatype wants an 'Int' for the style, which is supposed to
--   point into the 'styleSheetCellXfs' part of a stylesheet. However, this can
--   be difficult to work with, as it requires manual tracking of cell style
--   IDs, which in turns requires manual tracking of font IDs, border IDs, etc.
-- * Row-span and column-span properties are set on the worksheet as a whole
--   ('wsMerges') rather than on individual cells.
-- * Excel does not correctly deal with borders on cells that span multiple
--   columns or rows. Instead, these rows must be set on all the edge cells
--   in the block. Again, this means that this becomes a global property of
--   the spreadsheet rather than properties of individual cells.
--
-- This function deals with all these problems. Given a map of 'FormattedCell's,
-- which refer directly to 'Font's, 'Border's, etc. (rather than font IDs,
-- border IDs, etc.), and an initial stylesheet, it recovers all possible
-- sharing, constructs IDs, and then constructs the final 'CellMap', as well as
-- the final stylesheet and list of merges.
--
-- If you don't already have a 'StyleSheet' you want to use as starting point
-- then 'minimalStyleSheet' is a good choice.
formatted ::
  Map (RowIndex, ColumnIndex) FormattedCell -> StyleSheet -> Formatted
formatted cs styleSheet =
  let initSt = stateFromStyleSheet styleSheet
      (cs', finalSt) = runState (mapM (uncurry formatCell) (M.toList cs)) initSt
      styleSheet' = updateStyleSheetFromState styleSheet finalSt
   in Formatted
        { formattedCellMap = M.fromList (concat cs'),
          formattedStyleSheet = styleSheet',
          formattedMerges = reverse (finalSt ^. #formattingMerges)
        }

-- | Build an 'Xlsx', render provided cells as per the 'StyleSheet'.
formatWorkbook ::
  [(Text, Map (RowIndex, ColumnIndex) FormattedCell)] -> StyleSheet -> Xlsx
formatWorkbook nfcss initStyle = extract go
  where
    initSt = stateFromStyleSheet initStyle
    go = flip runState initSt $
      forM nfcss $ \(name, fcs) -> do
        cs' <- forM (M.toList fcs) $ \(rc, fc) -> formatCell rc fc
        merges <- reverse . formattingMerges <$> get
        return
          ( name,
            def
              & wsCells
              .~ M.fromList (concat cs')
              & wsMerges
              .~ merges
          )
    extract (sheets, st) =
      def
        & xlSheets
        .~ sheets
        & xlStyles
        .~ renderStyleSheet (updateStyleSheetFromState initStyle st)

-- | reverse to 'formatted' which allows to get a map of formatted cells
-- from an existing worksheet and its workbook's style sheet
toFormattedCells ::
  CellMap -> [Range] -> StyleSheet -> Map (RowIndex, ColumnIndex) FormattedCell
toFormattedCells m merges StyleSheet {..} = applyMerges $ M.map toFormattedCell m
  where
    toFormattedCell cell@Cell {..} =
      FormattedCell
        { formattedCell = cell {cellStyle = Nothing}, -- just to remove confusion
          formattedFormat =
            maybe def formatFromStyle $ flip M.lookup cellXfs =<< cellStyle,
          formattedColSpan = 1,
          formattedRowSpan = 1
        }
    formatFromStyle cellXf =
      Format
        { formatAlignment = applied cellXfApplyAlignment cellXfAlignment cellXf,
          formatBorder =
            flip M.lookup borders
              =<< applied cellXfApplyBorder cellXfBorderId cellXf,
          formatFill =
            flip M.lookup fills
              =<< applied cellXfApplyFill cellXfFillId cellXf,
          formatFont =
            flip M.lookup fonts
              =<< applied cellXfApplyFont cellXfFontId cellXf,
          formatNumberFormat =
            lookupNumFmt
              =<< applied cellXfApplyNumberFormat cellXfNumFmtId cellXf,
          formatProtection = cellXfProtection cellXf,
          formatPivotButton = cellXfPivotButton cellXf,
          formatQuotePrefix = cellXfQuotePrefix cellXf
        }
    idMapped :: [a] -> Map Int a
    idMapped = M.fromList . zip [0 ..]
    cellXfs = idMapped styleSheetCellXfs
    borders = idMapped styleSheetBorders
    fills = idMapped styleSheetFills
    fonts = idMapped styleSheetFonts
    lookupNumFmt fId =
      asum
        [ StdNumberFormat <$> idToStdNumberFormat fId,
          UserNumberFormat <$> M.lookup fId styleSheetNumFmts
        ]
    applied :: (CellXf -> Maybe Bool) -> (CellXf -> Maybe a) -> CellXf -> Maybe a
    applied applyProp prop cXf = do
      apply <- applyProp cXf
      if apply then prop cXf else fail "not applied"
    applyMerges cells = foldl' onlyTopLeft cells merges
    onlyTopLeft cells range = flip execState cells $ do
      let ((r1, c1), (r2, c2)) =
            fromJustNote "fromRange" $ fromRange range
          nonTopLeft = tail [(r, c) | r <- [r1 .. r2], c <- [c1 .. c2]]
      forM_ nonTopLeft (modify . M.delete)
      at (r1, c1)
        . non def
        . #formattedRowSpan
        .= (unRowIndex r2 - unRowIndex r1 + 1)
      at (r1, c1)
        . non def
        . #formattedColSpan
        .= (unColumnIndex c2 - unColumnIndex c1 + 1)

data CondFormatted = CondFormatted
  { -- | The resulting stylesheet
    condformattedStyleSheet :: StyleSheet,
    -- | The final map of conditional formatting rules applied to ranges
    condformattedFormattings :: Map SqRef ConditionalFormatting
  }
  deriving (Eq, Show, Generic)

conditionallyFormatted ::
  Map CellRef [FormattedCondFmt] -> StyleSheet -> CondFormatted
conditionallyFormatted cfs styleSheet =
  CondFormatted
    { condformattedStyleSheet = styleSheet & #styleSheetDxfs .~ finalDxfs,
      condformattedFormattings = fmts
    }
  where
    (cellFmts, dxf2id) = runState (mapM (mapM mapDxf) cfs) dxf2id0
    dxf2id0 = fromValueList (styleSheet ^. #styleSheetDxfs)
    fmts =
      M.fromList
        . map mergeSqRef
        . groupBy ((==) `on` snd)
        . sortBy (comparing snd)
        $ M.toList cellFmts
    mergeSqRef cellRefs2fmt =
      ( SqRef (map fst cellRefs2fmt),
        headNote "fmt group should not be empty" (map snd cellRefs2fmt)
      )
    finalDxfs = toValueList dxf2id

{-------------------------------------------------------------------------------
  Implementation details
-------------------------------------------------------------------------------}

-- | Format a cell with (potentially) rowspan or colspan
formatCell ::
  (RowIndex, ColumnIndex) ->
  FormattedCell ->
  State FormattingState [((RowIndex, ColumnIndex), Cell)]
formatCell (row, col) cell = do
  let (block, mMerge) = cellBlock (row, col) cell
  forM_ mMerge $ \merge -> #formattingMerges %= (:) merge
  mapM go block
  where
    go ::
      ((RowIndex, ColumnIndex), FormattedCell) ->
      State FormattingState ((RowIndex, ColumnIndex), Cell)
    go (pos, c@FormattedCell {..}) = do
      styleId <- cellStyleId c
      return (pos, formattedCell {cellStyle = styleId})

-- | Cell block corresponding to a single 'FormattedCell'
--
-- A single 'FormattedCell' might have a colspan or rowspan greater than 1.
-- Although Excel obviously supports cell merges, it does not correctly apply
-- borders to the cells covered by the rowspan or colspan. Therefore we create
-- a block of cells in this function; the top-left is the cell proper, and the
-- remaining cells are the cells covered by the rowspan/colspan.
--
-- Also returns the cell merge instruction, if any.
cellBlock ::
  (RowIndex, ColumnIndex) ->
  FormattedCell ->
  ([((RowIndex, ColumnIndex), FormattedCell)], Maybe Range)
cellBlock (row, col) cell@FormattedCell {..} = (block, merge)
  where
    block :: [((RowIndex, ColumnIndex), FormattedCell)]
    block =
      [ ((row', col'), cellAt (row', col'))
        | row' <- [topRow .. bottomRow],
          col' <- [leftCol .. rightCol]
      ]

    merge :: Maybe Range
    merge = do
      guard (topRow /= bottomRow || leftCol /= rightCol)
      return $ mkRange (topRow, leftCol) (bottomRow, rightCol)

    cellAt :: (RowIndex, ColumnIndex) -> FormattedCell
    cellAt (row', col') =
      if row' == row && col == col'
        then cell
        else def & #formattedFormat . #formatBorder ?~ borderAt (row', col')

    border = formatBorder formattedFormat

    borderAt :: (RowIndex, ColumnIndex) -> Border
    borderAt (row', col') =
      def
        & #borderTop
        .~ do guard (row' == topRow); borderTop =<< border
        & #borderBottom
        .~ do guard (row' == bottomRow); borderBottom =<< border
        & #borderLeft
        .~ do guard (col' == leftCol); borderLeft =<< border
        & #borderRight
        .~ do guard (col' == rightCol); borderRight =<< border

    topRow, bottomRow :: RowIndex
    leftCol, rightCol :: ColumnIndex
    topRow = row
    bottomRow = RowIndex $ unRowIndex row + formattedRowSpan - 1
    leftCol = col
    rightCol = ColumnIndex $ unColumnIndex col + formattedColSpan - 1

cellStyleId :: FormattedCell -> State FormattingState (Maybe Int)
cellStyleId c = mapM (getId #formattingCellXfs) =<< constructCellXf c

constructCellXf :: FormattedCell -> State FormattingState (Maybe CellXf)
constructCellXf FormattedCell {formattedFormat = Format {..}} = do
  mBorderId <- getId #formattingBorders `mapM` formatBorder
  mFillId <- getId #formattingFills `mapM` formatFill
  mFontId <- getId #formattingFonts `mapM` formatFont
  let getFmtId ::
        Lens' FormattingState (Map Text Int) ->
        NumberFormat ->
        State FormattingState Int
      getFmtId _ (StdNumberFormat fmt) = return (stdNumberFormatId fmt)
      getFmtId l (UserNumberFormat fmt) = getId' firstUserNumFmtId l fmt
  mNumFmtId <- getFmtId #formattingNumFmts `mapM` formatNumberFormat
  let xf =
        CellXf
          { cellXfApplyAlignment = apply formatAlignment,
            cellXfApplyBorder = apply mBorderId,
            cellXfApplyFill = apply mFillId,
            cellXfApplyFont = apply mFontId,
            cellXfApplyNumberFormat = apply formatNumberFormat,
            cellXfApplyProtection = apply formatProtection,
            cellXfBorderId = mBorderId,
            cellXfFillId = mFillId,
            cellXfFontId = mFontId,
            cellXfNumFmtId = mNumFmtId,
            cellXfPivotButton = formatPivotButton,
            cellXfQuotePrefix = formatQuotePrefix,
            cellXfId = Nothing, -- TODO
            cellXfAlignment = formatAlignment,
            cellXfProtection = formatProtection
          }
  return $ if xf == def then Nothing else Just xf
  where
    -- If we have formatting instructions, we want to set the corresponding
    -- applyXXX properties
    apply :: Maybe a -> Maybe Bool
    apply Nothing = Nothing
    apply (Just _) = Just True

mapDxf :: FormattedCondFmt -> State (Map Dxf Int) CfRule
mapDxf FormattedCondFmt {..} = do
  dxf2id <- get
  dxfId <- case M.lookup condfmtDxf dxf2id of
    Just i ->
      return i
    Nothing -> do
      let newId = M.size dxf2id
      modify $ M.insert condfmtDxf newId
      return newId
  return
    CfRule
      { cfrCondition = condfmtCondition,
        cfrDxfId = Just dxfId,
        cfrPriority = condfmtPriority,
        cfrStopIfTrue = condfmtStopIfTrue
      }
