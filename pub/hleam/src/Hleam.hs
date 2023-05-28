module Hleam (main, runParser) where

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
import Hleam.Import

main :: IO ()
main = pure ()

runParser :: String -> ParseResult (Located HsModule)
runParser src =
  unP parseModule parseState
  where
    filename = "<interactive>" :: String
    location = mkRealSrcLoc (mkFastString filename) 1 1
    parseState = initParserState parserOpts (stringToStringBuffer src) location

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
