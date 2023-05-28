module HleamSpec (spec) where

import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import Hleam
import Hleam.Import
import Hleam.Renderer
import Hleam.Transpiler
import Test.Hspec

spec :: Spec
spec =
  it "Trivial parser" $ do
    src <- readFile "test/LanguageCodes.hs"
    case runParser src of
      POk _ ast -> do
        putStrLn ("\n======" :: Text)
        putStrLn . show @Text . newMod $ unLoc ast
        putStrLn ("======" :: Text)
        putStrLn . renMod . newMod $ unLoc ast
      PFailed {} -> error "GHC parser failure!"
    True `shouldBe` True
