module Functora.Elm2Miso
  ( main,
    elm2miso,
  )
where

import System.Environment
import Text.Regex
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile, outFile] -> do
      inString <- readFile inFile
      let outString = elm2miso inString
      writeFile outFile outString
    _ -> putStrLn "usage: Translator <inFile> <outFile>"

elm2miso :: String -> String
elm2miso raw =
  foldr
    ( \(reg, rep) s ->
        subRegex reg s rep
    )
    raw
    replacements

-- only works on single lines!
replacements :: [(Regex, String)]
replacements =
  [ (mkRegex "List\\.concat", "Prelude.concat"),
    (mkRegex "Svg\\.Attributes\\.Miso\\.class\\_)", "Svg.class_'"),
    (mkRegex "<< Decode\\.succeed)", "Miso.emptyDecoder << const)"),
    (mkRegex "Miso\\.events\\.on", "Miso.on"),
    (mkRegex "Miso\\.Attributes\\.attribute", "Miso.textProp"),
    (mkRegex "data alias", "type"),
    (mkRegex "List\\.map", "Prelude.map"),
    (mkRegex "List\\.filterMap identity", "Maybe.mapMaybe id"),
    (mkRegex "Miso\\.map", "fmap"),
    (mkRegex "Maybe\\.map", "fmap"),
    (mkRegex "Maybe\\.withDefault", "Maybe.maybe"),
    (mkRegex "\\(\\((.*)\\) as config_", "(config_@\\1"),
    (mkRegex "Config \\{ config_ \\|", "config_ {"),
    (mkRegex "type ", "data "),
    (mkRegex "\\(Config config_\\)", "config_"),
    (mkRegex "Html msg", "Miso.View msg"),
    (mkRegex "Miso\\.Attibute\\.attribute ", "Miso.textProp "),
    (mkRegex "Miso\\.node ", "Miso.nodeHtml "),
    (mkRegex "Miso\\.td ", "Miso.td_"),
    (mkRegex "Miso\\.th ", "Miso.th_"),
    (mkRegex "Miso\\.table ", "Miso.table_"),
    (mkRegex "Miso\\.a ", "Miso.a_"),
    (mkRegex "Miso\\.input ", "Miso.input_ "),
    (mkRegex "Miso\\.div ", "Miso.div_ "),
    (mkRegex "Miso\\.span ", "Miso.span_ "),
    (mkRegex "Miso\\.i ", "Miso.i_ "),
    (mkRegex "Miso\\.label ", "Miso.label_ "),
    (mkRegex "Miso\\.td ", "Miso.td_ "),
    (mkRegex "Miso\\.th ", "Miso.th_ "),
    (mkRegex "Miso\\.tr ", "Miso.tr_ "),
    (mkRegex "Miso\\.thead ", "Miso.thead_ "),
    (mkRegex "Miso\\.tbody ", "Miso.tbody_ "),
    (mkRegex "class ", "Miso.class_ "),
    (mkRegex "Miso\\.table ", "Miso.table_ "),
    (mkRegex "Html\\.", "Miso."),
    (mkRegex "List \\(([^\\)]*)\\)", "[\\1]"),
    (mkRegex " \\$: ", " :: "),
    (mkRegex " :: ", " : "),
    (mkRegex " : ", " $: "),
    (mkRegex "exposing", "")
  ]
