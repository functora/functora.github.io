{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Weverything #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-safe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
#endif

import qualified Data.Text as T
import Distribution.Simple hiding (Module (..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Utils (writeUTF8File)
import Functora.Prelude hiding (empty)
import qualified System.Directory as Directory
import System.Environment (getProgName)
import qualified Text.Casing as Casing

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = \p l h f ->
          codeGenHook l
            >> buildHook simpleUserHooks p l h f
      }

codeGenHook :: LocalBuildInfo -> IO ()
codeGenHook _ = do
  prog <- getProgName
  cssRaw <- liftIO $ Directory.listDirectory "dist/themes/"
  let cssKebab =
        sort . fmap (dropEnd 8) $ filter (isSuffixOf ".min.css") cssRaw
  let cssPascal =
        fmap Casing.pascal cssKebab
  if cssKebab == fmap Casing.kebab cssPascal
    then putStrLn $ inspect @Text cssPascal
    else
      error
        $ "Bad kebab <-> pascal isomorphism in "
        <> inspect @Text cssKebab
  liftIO
    . writeFileIfChanged
    $ generateCode prog cssPascal

generateCode :: String -> [String] -> String
generateCode prog css =
  intercalate
    "\n"
    [ "{- DO NOT EDIT. This file was auto-generated by the "
        <> prog
        <> " program. -}",
      "module Functora.Miso.Theme (Theme(..)) where",
      "import Prelude",
      "import Data.Data",
      "import Data.Binary",
      "import GHC.Generics",
      "data Theme = " <> intercalate "\n | " css,
      " deriving stock (Eq, Ord, Show, Read, Data, Generic, Enum, Bounded)",
      "instance Binary Theme"
    ]

writeFileIfChanged :: String -> IO ()
writeFileIfChanged newFile = do
  oldFile <-
    (Just . T.unpack <$> readFile doNotEditFilePath)
      `catchAny` const (pure Nothing)
  when (oldFile /= Just newFile)
    $ writeUTF8File doNotEditFilePath newFile

doNotEditFilePath :: FilePath
doNotEditFilePath =
  "src/Functora/Miso/Theme.hs"
