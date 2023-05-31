module SombraSpec (spec) where

import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import Sombra
import Sombra.Import
import Sombra.Renderer
import Sombra.Transpiler
import Test.Hspec

spec :: Spec
spec = do
  focus . it "Main" $ do
    main
    True `shouldBe` True
  it "Trivial parser" $ do
    src <- readFile "test/LanguageCodes.hs"
    case runParser src of
      POk _ astHs -> do
        let astGl = newMod $ unLoc astHs
        putStrLn ("\n======" :: Text)
        putStrLn $ show @Text astGl
        let renGl = renMod astGl
        putStrLn ("\n======" :: Text)
        putStrLn renGl
        res <- fmt renGl
        putStrLn ("======" :: Text)
        putStrLn $ fromEither res
        True `shouldBe` True
      PFailed {} ->
        error "GHC parser failure!"
