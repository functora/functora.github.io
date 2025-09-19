{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_, replicateM)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String (fromString)
import Hakyll
import Main.Utf8 (withUtf8)
import System.FilePath

main :: IO ()
main = withUtf8 . hakyllWith cfg $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "fontawesome-free-5.12.1-web/css/**" $ do
    route idRoute
    compile compressCssCompiler
  match "fontawesome-free-5.12.1-web/webfonts/**" $ do
    route idRoute
    compile copyFileCompiler
  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler
  match "favicon/*" $ do
    route $ gsubRoute "favicon/" (const "")
    compile copyFileCompiler
  match "eval.html" $ do
    route idRoute
    compile copyFileCompiler
  match "index.html" $
    newIndex Informal
  match "index.html" . version "formal" $
    newIndex Formal
  create
    [ "about.html",
      "skills.html",
      "code.html",
      "blog.html",
      "contact.html"
    ]
    $ do
      route idRoute
      compile $ do
        anchor <- takeBaseName . toFilePath <$> getUnderlying
        let ctx = constField "anchor" anchor <> defaultContext
        makeItem ""
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/index-redirect.html" ctx
          >>= relativizeUrls
  match "license.markdown" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
  match "blog/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
  create ["blog-archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "blog/*"
      let blogCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Blog Archive"
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog-archive.html" blogCtx
        >>= loadAndApplyTemplate "templates/default.html" blogCtx
        >>= relativizeUrls
  match "bip39/*" $ do
    route idRoute
    compile copyFileCompiler
  forM_ colors $ \mcolor -> do
    let color = fromMaybe "black" mcolor
    let name =
          "bip39/calculator"
            <> maybe mempty ("-" <>) mcolor
            <> ".html"
    create [fromString name] $ do
      let defCtx =
            mkStyleCtx Formal
              <> constField "table-color" color
      route idRoute
      compile $ do
        let idxs items label =
              listField
                label
                ( field "idx" (pure . itemBody)
                    <> defCtx
                )
                ( do
                    xs <- mapM (makeItem . ("\8470" <>) . show) items
                    x <- makeItem mempty
                    pure $ x : xs
                )
        let rows top label =
              listField
                label
                ( listFieldWith
                    "cols"
                    ( field "col" (pure . itemBody)
                        <> defCtx
                    )
                    ( \item -> do
                        let (idx, raw) = itemBody item
                        h <-
                          makeItem . (\x -> "<b>" <> x <> "</b>") $
                            if idx <= 11
                              then "bit " <> show idx
                              else raw
                        xs <-
                          replicateM 11 . makeItem $
                            if idx <= 11
                              then raw
                              else mempty
                        t <-
                          makeItem $
                            if (top || idx < 4) && idx <= 11
                              then raw
                              else mempty
                        pure $ [h] <> xs <> [t]
                    )
                )
                ( mapM makeItem
                    . zip [1 ..]
                    . (<> ["sum=", "sum+1=", "word="])
                    $ fmap (show . (2 ^)) [0 .. 10]
                )
        let ctx =
              idxs [1 .. 12] "idxs-top"
                <> idxs [13 .. 24] "idxs-bottom"
                <> rows True "rows-top"
                <> rows False "rows-bottom"
                <> constField "title" "BIP39 Dice Calculator"
                <> defCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/bip39-dice-calculator.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
  match "templates/*" $ compile templateBodyCompiler
  match "license.markdown" $ compile pandocCompiler
  match "index/*.markdown" $ compile pandocCompiler
  match "index/*.html" $
    compile $ do
      posts <- recentFirst =<< loadAll "blog/*"
      meta <- getMetadata =<< getUnderlying
      pairs <-
        mapM makeItem
          . makePairs
          . fromMaybe mempty
          $ lookupStringList "pairs" meta
      let pairCtx =
            field "left" (return . fst . itemBody)
              <> field "right" (return . snd . itemBody)
      let ctx =
            listField "posts" postCtx (return posts)
              <> listField "pairs" pairCtx (return pairs)
              <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= relativizeUrls

data Style
  = Informal
  | Formal
  deriving (Eq, Ord, Show)

cfg :: Configuration
cfg =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

makePairs :: [a] -> [(a, a)]
makePairs =
  reverse . this []
  where
    this acc [] = acc
    this acc (x0 : x1 : xs) = this ((x0, x1) : acc) xs

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext

newIndex :: Style -> Rules ()
newIndex style = do
  route
    . constRoute
    $ case style of
      Informal -> "index.html"
      Formal -> "formal.html"
  compile $ do
    blocks <- chronological =<< loadAll "index/*"
    let indexCtx =
          listField "blocks" (mkStyleCtx style) (return blocks)
            <> mkStyleCtx style
    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls

--
-- TODO : add when condition for formal stuff, generate docx
--
mkStyleCtx :: Style -> Context String
mkStyleCtx style =
  case style of
    Informal ->
      defaultContext
    Formal ->
      constField "formal" "true"
        <> constField "color" "white"
        <> defaultContext

colors :: [Maybe String]
colors =
  [ Nothing,
    Just "red",
    Just "green",
    Just "blue",
    Just "cyan",
    Just "magenta"
  ]
