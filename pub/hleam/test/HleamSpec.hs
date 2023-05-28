module HleamSpec (spec) where

import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import Hleam
import Hleam.Import
import Hleam.Trans
import Test.Hspec

spec :: Spec
spec =
  it "Trivial parser" $ do
    case runParser ("module Foo () where\nplus1 :: Int -> String\nplus1 x = show x" :: String) of
      POk _ ast0 -> do
        let ast = unLoc ast0
        putStrLn ("\n======\nGHC parser result:\n" :: Text)
        putStrLn . showPprUnsafe $ ppr ast
        putStrLn ("\n======\nTranspiler result:\n" :: Text)
        putStrLn . show @Text $ newMod ast
      PFailed {} ->
        error "GHC parser failure!"
    True `shouldBe` True
