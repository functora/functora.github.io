{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Xlsx.Types.Cell
  ( CellFormula (..),
    FormulaExpression (..),
    simpleCellFormula,
    sharedFormulaByIndex,
    SharedFormulaIndex (..),
    SharedFormulaOptions (..),
    formulaDataFromCursor,
    applySharedFormulaOpts,
    Cell (..),
    CellMap,
  )
where

import Control.Arrow (first)
#ifdef USE_MICROLENS
import Lens.Micro.TH (makeLenses)
#else
import Control.Lens.TH (makeLenses)
#endif
import Codec.Xlsx.Parser.Internal
import Codec.Xlsx.Types.Comment
import Codec.Xlsx.Types.Common
import Codec.Xlsx.Writer.Internal
import Control.DeepSeq (NFData)
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.XML
import Text.XML.Cursor

-- | Formula for the cell.
--
-- TODO: array, dataTable formula types support
--
-- See 18.3.1.40 "f (Formula)" (p. 1636)
data CellFormula = CellFormula
  { cellfExpression :: FormulaExpression,
    -- | Specifies that this formula assigns a value to a name.
    cellfAssignsToName :: Bool,
    -- | Indicates that this formula needs to be recalculated
    -- the next time calculation is performed.
    -- [/Example/: This is always set on volatile functions,
    -- like =RAND(), and circular references. /end example/]
    cellfCalculate :: Bool
  }
  deriving (Eq, Show, Generic)

instance NFData CellFormula

-- | formula type with type-specific options
data FormulaExpression
  = NormalFormula Formula
  | SharedFormula SharedFormulaIndex
  deriving (Eq, Show, Generic)

instance NFData FormulaExpression

defaultFormulaType :: Text
defaultFormulaType = "normal"

-- | index of shared formula in worksheet's 'wsSharedFormulas'
-- property
newtype SharedFormulaIndex = SharedFormulaIndex Int
  deriving (Eq, Ord, Show, Generic)

instance NFData SharedFormulaIndex

data SharedFormulaOptions = SharedFormulaOptions
  { _sfoRef :: CellRef,
    _sfoExpression :: Formula
  }
  deriving (Eq, Show, Generic)

instance NFData SharedFormulaOptions

simpleCellFormula :: Text -> CellFormula
simpleCellFormula expr =
  CellFormula
    { cellfExpression = NormalFormula $ Formula expr,
      cellfAssignsToName = False,
      cellfCalculate = False
    }

sharedFormulaByIndex :: SharedFormulaIndex -> CellFormula
sharedFormulaByIndex si =
  CellFormula
    { cellfExpression = SharedFormula si,
      cellfAssignsToName = False,
      cellfCalculate = False
    }

-- | Currently cell details include cell values, style ids and cell
-- formulas (inline strings from @\<is\>@ subelements are ignored)
data Cell = Cell
  { cellStyle :: Maybe Int,
    cellValue :: Maybe CellValue,
    cellComment :: Maybe Comment,
    cellFormula :: Maybe CellFormula
  }
  deriving (Eq, Show, Generic)

instance NFData Cell

instance Default Cell where
  def = Cell Nothing Nothing Nothing Nothing

-- | Map containing cell values which are indexed by row and column
-- if you need to use more traditional (x,y) indexing please you could
-- use corresponding accessors from ''Codec.Xlsx.Lens''
type CellMap = Map (RowIndex, ColumnIndex) Cell

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

formulaDataFromCursor ::
  Cursor -> [(CellFormula, Maybe (SharedFormulaIndex, SharedFormulaOptions))]
formulaDataFromCursor cur = do
  cellfAssignsToName <- fromAttributeDef "bx" False cur
  cellfCalculate <- fromAttributeDef "ca" False cur
  t <- fromAttributeDef "t" defaultFormulaType cur
  (cellfExpression, shared) <-
    case t of
      d | d == defaultFormulaType -> do
        formula <- fromCursor cur
        return (NormalFormula formula, Nothing)
      "shared" -> do
        let expr = listToMaybe $ fromCursor cur
        ref <- maybeAttribute "ref" cur
        si <- fromAttribute "si" cur
        return
          ( SharedFormula si,
            (,)
              <$> pure si
              <*> (SharedFormulaOptions <$> ref <*> expr)
          )
      _ ->
        fail $ "Unexpected formula type" ++ show t
  return (CellFormula {..}, shared)

instance FromAttrVal SharedFormulaIndex where
  fromAttrVal = fmap (first SharedFormulaIndex) . fromAttrVal

instance FromAttrBs SharedFormulaIndex where
  fromAttrBs = fmap SharedFormulaIndex . fromAttrBs

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

instance ToElement CellFormula where
  toElement nm CellFormula {..} =
    formulaEl {elementAttributes = elementAttributes formulaEl <> commonAttrs}
    where
      commonAttrs =
        M.fromList $
          catMaybes
            [ "bx" .=? justTrue cellfAssignsToName,
              "ca" .=? justTrue cellfCalculate,
              "t" .=? justNonDef defaultFormulaType fType
            ]
      (formulaEl, fType) =
        case cellfExpression of
          NormalFormula f -> (toElement nm f, defaultFormulaType)
          SharedFormula si -> (leafElement nm ["si" .= si], "shared")

instance ToAttrVal SharedFormulaIndex where
  toAttrVal (SharedFormulaIndex si) = toAttrVal si

applySharedFormulaOpts :: SharedFormulaOptions -> Element -> Element
applySharedFormulaOpts SharedFormulaOptions {..} el =
  el
    { elementAttributes = elementAttributes el <> M.fromList ["ref" .= _sfoRef],
      elementNodes = NodeContent (unFormula _sfoExpression) : elementNodes el
    }
