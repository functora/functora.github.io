module Dazzle (main, runParser) where

import qualified Data.Text as T
import Dazzle.Import
import Dazzle.Renderer
import Dazzle.Transpiler
import GHC.Data.EnumSet
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Flags
import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Parser
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Outputable
import Ormolu (PrinterOpts (..))
import qualified Ormolu
import qualified Ormolu.Config as Ormolu

main :: IO ()
main = withUtf8 $ do
  src <- ormoluFmt =<< readFile "test/LanguageCodes.hs"
  case runParser src of
    POk _ astHs -> do
      let astGl = newMod $ unLoc astHs
      let renGl = renMod astGl
      putStrLn renGl
    PFailed {} ->
      error "GHC parser failure!"

runParser :: Text -> ParseResult (Located HsModule)
runParser src =
  unP parseModule parseState
  where
    filename =
      "<interactive>" :: String
    location =
      mkRealSrcLoc (mkFastString filename) 1 1
    parseState =
      initParserState parserOpts (stringToStringBuffer $ T.unpack src) location

parserOpts :: ParserOpts
parserOpts =
  mkParserOpts
    (mempty :: EnumSet Extension) -- permitted language extensions enabled
    (diagOpts :: DiagOpts) -- diagnostic options
    (mempty :: [String]) -- Supported Languages and Extensions
    (False :: Bool) -- are safe imports on?
    (True :: Bool) -- keeping Haddock comment tokens
    (True :: Bool) --	keep regular comment tokens
    (True :: Bool) -- If this is enabled, '{-# LINE ... -#}' and '{-# COLUMN ... #-}' update the internal position kept by the parser. Otherwise, those pragmas are lexed as ITline_prag and ITcolumn_prag tokens.

diagOpts :: DiagOpts
diagOpts =
  DiagOpts
    { diag_warning_flags = mempty :: EnumSet WarningFlag, -- Enabled warnings
      diag_fatal_warning_flags = mempty :: EnumSet WarningFlag, -- Fatal warnings
      diag_warn_is_error = False :: Bool, -- Treat warnings as errors
      diag_reverse_errors = False :: Bool, -- Reverse error reporting order
      diag_max_errors = Nothing :: Maybe Int, -- Max reported error count
      diag_ppr_ctx = defaultSDocContext :: SDocContext -- Error printing context
    }

--
-- TODO : really need hlint, not ormolu
--
ormoluFmt :: Text -> IO Text
ormoluFmt =
  Ormolu.ormolu ormoluCfg "<interactive>"
    . T.unpack

ormoluCfg :: Ormolu.Config Ormolu.RegionIndices
ormoluCfg =
  Ormolu.defaultConfig
    { Ormolu.cfgPrinterOpts =
        Ormolu.defaultPrinterOpts
          { poIndentation = pure 2,
            -- poColumnLimit = pure NoLimit,
            -- poFunctionArrows = pure TrailingArrows,
            poCommaStyle = pure Ormolu.Trailing,
            poImportExportStyle = pure Ormolu.ImportExportTrailing,
            poIndentWheres = pure True,
            poRecordBraceSpace = pure True,
            poNewlinesBetweenDecls = pure 1,
            poHaddockStyle = pure Ormolu.HaddockSingleLine,
            poHaddockStyleModule =
              pure $
                Ormolu.PrintStyleOverride Ormolu.HaddockSingleLine,
            poLetStyle = pure Ormolu.LetInline,
            poInStyle = pure Ormolu.InRightAlign,
            -- poSingleConstraintParens = pure ConstraintAlways,
            -- poUnicode = pure UnicodeNever,
            poRespectful = pure False
          }
    }
