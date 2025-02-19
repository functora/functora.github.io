{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module StreamTests
  ( tests,
  )
where

#ifdef USE_MICROLENS

import Data.Generics.Labels
import Test.Tasty (TestName, TestTree, testGroup)
tests :: TestTree
tests = testGroup
  "I stubbed out the tests module for microlens \
   because it doesn't understand setOf. \
   Volunteers are welcome to fix this!"
    []
#else

import Control.Exception
import "zip" Codec.Archive.Zip as Zip
import Codec.Xlsx
import Codec.Xlsx.Parser.Stream
import Conduit ((.|))
import qualified Conduit as C
import Control.Lens hiding (indexed)
import Control.Monad (void)
import Data.Set.Lens
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Diff
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TestXlsx
import qualified Codec.Xlsx.Writer.Stream as SW
import qualified Codec.Xlsx.Writer.Internal.Stream as SW
import Control.Monad (void)
import Control.Monad.State.Lazy
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series.Instances ()
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Printf
import Data.Conduit

tshow :: Show a => a -> Text
tshow = Text.pack . show

toBs :: Xlsx -> BS.ByteString
toBs = LB.toStrict . fromXlsx testTime

tests :: TestTree
tests =
  testGroup "Stream tests"
    [
      testGroup "Writer/shared strings"
      [ testProperty "Input same as the output" sharedStringInputSameAsOutput
      , testProperty "Set of input texts is same as map length" sharedStringInputTextsIsSameAsMapLength
      , testProperty "Set of input texts is as value set length" sharedStringInputTextsIsSameAsValueSetLength
      ],

      testGroup "Reader/shared strings"
      [ testCase "Can parse RichText values" richCellTextIsParsed
      ],


      testGroup "Reader/Writer"
      [ testCase "Write as stream, see if memory based implementation can read it" $ readWrite simpleWorkbook
      , testCase "Write as stream, see if memory based implementation can read it" $ readWrite simpleWorkbookRow
      , testCase "Test a small workbook which has a fullblown sqaure" $ readWrite smallWorkbook
      , testCase "Test a big workbook as a full square which caused issues with zipstream \
                 The buffer of zipstream maybe 1kb, this workbook is big enough \
                 to be more than that. \
                 So if this encodes/decodes we know we can handle those sizes. \
                 In some older version the bytestring got cut off resulting in a corrupt xlsx file"
                  $ readWrite bigWorkbook
      -- , testCase "Write as stream, see if memory based implementation can read it" $ readWrite testXlsx
      -- TODO forall SheetItem write that can be read
      ],

      testGroup "Reader/inline strings"
      [ testCase "Can parse row with inline strings" inlineStringsAreParsed
      ],

      testGroup "Reader/floats parsing"
      [ testCase "Can parse untyped values as floats" untypedCellsAreParsedAsFloats
      ]
    ]

readWrite :: Xlsx -> IO ()
readWrite input = do
  BS.writeFile "testinput.xlsx" (toBs input)
  items <- fmap (toListOf (traversed . #si_row)) $ runXlsxM "testinput.xlsx" $ collectItems $ makeIndex 1
  bs <- runConduitRes $ void (SW.writeXlsx SW.defaultSettings $ C.yieldMany items) .| C.foldC
  case toXlsxEither $ LB.fromStrict bs of
    Right result  ->
      input @==?  result
    Left x -> do
      throwIO x

-- test if the input text is also the result (a property we use for convenience)
sharedStringInputSameAsOutput :: Text -> Either String String
sharedStringInputSameAsOutput someText =
  if someText  == out then Right msg  else Left msg
  where
    out = fst $ evalState (SW.upsertSharedString someText) SW.initialSharedString
    msg = printf "'%s' = '%s'" (Text.unpack out) (Text.unpack someText)

-- test if unique strings actually get set in the map as keys
sharedStringInputTextsIsSameAsMapLength :: [Text] -> Bool
sharedStringInputTextsIsSameAsMapLength someTexts =
    length result == length unqTexts
  where
   result  :: Map Text Int
   result = view #string_map $ traverse SW.upsertSharedString someTexts `execState` SW.initialSharedString
   unqTexts :: Set Text
   unqTexts = Set.fromList someTexts

-- test for every unique string we get a unique number
sharedStringInputTextsIsSameAsValueSetLength :: [Text] -> Bool
sharedStringInputTextsIsSameAsValueSetLength someTexts =
    length result == length unqTexts
  where
   result  :: Set Int
   result = setOf (#string_map . traversed) $ traverse SW.upsertSharedString someTexts `execState` SW.initialSharedString
   unqTexts :: Set Text
   unqTexts = Set.fromList someTexts

-- can we do xx
simpleWorkbook :: Xlsx
simpleWorkbook = def & atSheet "Sheet1" ?~ sheet
  where
    sheet = toWs [ ((RowIndex 1, ColumnIndex 1), a1)
                 , ((RowIndex 1, ColumnIndex 2), #cellValue ?~ CellText "text at B1 Sheet1" $ def) ]

a1 :: Cell
a1 = #cellValue ?~ CellText "text at A1 Sheet1" $ #cellStyle ?~ 1 $ def

-- can we do x
--           x
simpleWorkbookRow :: Xlsx
simpleWorkbookRow = def & atSheet "Sheet1" ?~ sheet
  where
    sheet = toWs [ ((RowIndex 1, ColumnIndex 1), a1)
                 , ((RowIndex 2, ColumnIndex 1), #cellValue ?~ CellText "text at A2 Sheet1" $ def) ]

toWs :: [((RowIndex, ColumnIndex), Cell)] -> Worksheet
toWs x = set #wsCells (M.fromList x) def

-- can we do xxx
--           xxx
--           .
--           .
smallWorkbook :: Xlsx
smallWorkbook = def & atSheet "Sheet1" ?~ sheet
  where
    sheet = toWs $ [1..2] >>= \row ->
                  [((row,1), a1)
                  , ((row,2), def & #cellValue ?~ CellText ("text at B"<> tshow row <> " Sheet1"))
                  , ((row,3), def & #cellValue ?~ CellText "text at C1 Sheet1")
                  , ((row,4), def & #cellValue ?~ CellDouble (0.2 + 0.1))
                  , ((row,5), def & #cellValue ?~ CellBool False)
                  ]
--    sheets = [("Sheet1" , toWs $ [1..2] >>= \row ->
--        [ ((RowIndex row, ColumnIndex 1), a1)
--        , ((RowIndex row, ColumnIndex 2),
--            def & cellValue ?~ CellText ("text at B"<> tshow row <> " Sheet1"))
--        , ((RowIndex row, ColumnIndex 3),
--            def & cellValue ?~ CellText "text at C1 Sheet1")
--        , ((RowIndex row, ColumnIndex 4),
--            def & cellValue ?~ CellDouble (0.2 + 0.1))
--        , ((RowIndex row, ColumnIndex 5),
--            def & cellValue ?~ CellBool False)
--        ]
--      )]

bigWorkbook :: Xlsx
bigWorkbook = def & atSheet "Sheet1" ?~ sheet
  where
    sheet = toWs $ [1..512] >>= \row ->
                  [((row,1), a1)
                  ,((row,2), def & #cellValue ?~ CellText ("text at B"<> tshow row <> " Sheet1"))
                  ,((row,3), def & #cellValue ?~ CellText "text at C1 Sheet1")
                  ]
--    sheets = [("Sheet1" , toWs $ [1..512] >>= \row ->
--        [((RowIndex row, ColumnIndex 1), a1)
--        ,((RowIndex row, ColumnIndex 2),
--            def & cellValue ?~ CellText ("text at B"<> tshow row <> " Sheet1"))
--        ,((RowIndex row, ColumnIndex 3),
--            def & cellValue ?~ CellText "text at C1 Sheet1")
--        ]
--      )]

inlineStringsAreParsed :: IO ()
inlineStringsAreParsed = do
  items <- runXlsxM "data/inline-strings.xlsx" $ collectItems $ makeIndex 1
  let expected =
        [ IM.fromList
            [ ( 1,
                Cell
                  { cellStyle = Nothing,
                    cellValue = Just (CellText "My Inline String"),
                    cellComment = Nothing,
                    cellFormula = Nothing
                  }
              ),
              ( 2,
                Cell
                  { cellStyle = Nothing,
                    cellValue = Just (CellText "two"),
                    cellComment = Nothing,
                    cellFormula = Nothing
                  }
              )
            ]
        ]
  expected @==? (items ^.. traversed . #si_row . #ri_cell_row)

untypedCellsAreParsedAsFloats :: IO ()
untypedCellsAreParsedAsFloats = do
  -- values in that file are under `General` cell-type and are not marked
  -- as numbers explicitly in `t` attribute.
  items <- runXlsxM "data/floats.xlsx" $ collectItems $ makeIndex 1
  let expected =
        [ IM.fromList [ (1, def & #cellValue ?~ CellDouble 12.0) ]
        , IM.fromList [ (1, def & #cellValue ?~ CellDouble 13.0) ]
        -- cell below has explicit `Numeric` type, while others are all `General`,
        -- but sometimes excel does not add a `t="n"` attr even to numeric cells
        -- but it should be default as number in any cases if `t` is missing
        , IM.fromList [ (1, def & #cellValue ?~ CellDouble 14.0 & #cellStyle ?~ 1 ) ]
        , IM.fromList [ (1, def & #cellValue ?~ CellDouble 15.0) ]
        ]
  expected @==? (ri_cell_row . si_row <$> items)


richCellTextIsParsed :: IO ()
richCellTextIsParsed = do
  BS.writeFile "testinput.xlsx" (toBs richWorkbook)
  runXlsxM "testinput.xlsx" $ do
    sharedStrings <- getOrParseSharedStringss
    let result = Set.fromList $ V.toList sharedStrings
    liftIO $ expected @==? result

  where
    expected :: Set.Set Text
    expected = Set.fromList
      [ textA1
      , firstClauseB1 <> secondClauseB1
      , firstClauseB2 <> secondClauseB2
      ]

    textA1 = "Text at A1"
    firstClauseB1 = "First clause at B1;"
    firstClauseB2 = "First clause at B2;"
    secondClauseB1 = "Second clause at B1"
    secondClauseB2 = "Second clause at B2"

    richWorkbook :: Xlsx
    richWorkbook = def & atSheet "Sheet1" ?~ toWs
      [ ((RowIndex 1, ColumnIndex 1), #cellValue ?~ CellText textA1 $ def)
      , ((RowIndex 2, ColumnIndex 1), #cellValue ?~ cellRich firstClauseB1 secondClauseB1 $ def)
      , ((RowIndex 2, ColumnIndex 2), #cellValue ?~ cellRich firstClauseB2 secondClauseB2 $ def)
      ]

cellRich :: Text -> Text -> CellValue
cellRich firstClause secondClause = CellRich
  [ RichTextRun
      { richTextRunProperties = Just RunProperties
          { runPropertiesBold = Nothing
          , runPropertiesCharset = Just 1
          , runPropertiesColor = Just Color
              { colorAutomatic = Nothing
              , colorARGB = Nothing
              , colorTheme = Just 1
              , colorTint = Nothing
              }
          , runPropertiesCondense = Nothing
          , runPropertiesExtend = Nothing
          , runPropertiesFontFamily = Just FontFamilySwiss
          , runPropertiesItalic = Nothing
          , runPropertiesOutline = Nothing
          , runPropertiesFont = Just "Aptos Narrow"
          , runPropertiesScheme = Nothing
          , runPropertiesShadow = Nothing
          , runPropertiesStrikeThrough = Nothing
          , runPropertiesSize = Just 11.0
          , runPropertiesUnderline = Nothing
          , runPropertiesVertAlign = Nothing
          }
      , richTextRunText = firstClause
      }
  , RichTextRun
      { richTextRunProperties = Just RunProperties
          { runPropertiesBold = Just True
          , runPropertiesCharset = Just 1
          , runPropertiesColor = Just Color
              { colorAutomatic = Nothing
              , colorARGB = Just "FFFF0000"
              , colorTheme = Nothing
              , colorTint = Nothing
              }
          , runPropertiesCondense = Nothing
          , runPropertiesExtend = Nothing
          , runPropertiesFontFamily = Just FontFamilySwiss
          , runPropertiesItalic = Nothing
          , runPropertiesOutline = Nothing
          , runPropertiesFont = Just "Arial"
          , runPropertiesScheme = Nothing
          , runPropertiesShadow = Nothing
          , runPropertiesStrikeThrough = Nothing
          , runPropertiesSize = Just 8.0
          , runPropertiesUnderline = Nothing
          , runPropertiesVertAlign = Nothing
          }
      , richTextRunText = secondClause
      }
  ]

#endif
