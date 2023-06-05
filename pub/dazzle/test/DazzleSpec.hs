module DazzleSpec (spec) where

import Dazzle
import Dazzle.Import
import Dazzle.Renderer
import Dazzle.Transpiler
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import Test.Hspec

spec :: Spec
spec = do
  focus . it "Main" $ do
    putStrLn @Text mempty
    putStrLn @Text mempty
    main
    putStrLn @Text mempty
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
