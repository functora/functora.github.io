module Main (main) where

import GHC.Parser
import Hleam.Main (main)

runParser :: ParserOpts -> String -> P a -> ParseResult a
runParser opts str parser = unP parser parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location
