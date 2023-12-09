------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options
import           System.FilePath
------------------------------------------------------------------------
-- Helper functions ----------------------------------------------------

-- | My own configuration for the website
config :: Configuration
config = defaultConfiguration
         { destinationDirectory = "docs"
         , previewPort          = 5000 }
-- --------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    `mappend` defaultContext

{- Include MathJax in the pandoc options so it is possible
to render pretty math equations. -}
pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
                { writerHTMLMathMethod = MathJax "" }

{- My personal website compiler with my preferences. -}
grassCompiler :: Compiler (Item String)
grassCompiler =
  pandocCompilerWith
  defaultHakyllReaderOptions
  pandocOptions

{- Used to process the main contents of the site. -}
baseFolderAndHtml :: Routes
baseFolderAndHtml =
  customRoute $ takeFileName . (`replaceExtension` "html") . toFilePath

removeMainFolder :: FilePath -> Routes
removeMainFolder path =
  customRoute $ ((++) path) . takeFileName . toFilePath

------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  match "templates/*" $ compile templateBodyCompiler

  match "main/images/*" $ do
      route   $ removeMainFolder "images/"
      compile copyFileCompiler

  match "main/extra/*" $ do
    route   $ removeMainFolder "docs/"
    compile copyFileCompiler

  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

  match "main/*" $ do
      route baseFolderAndHtml
      compile $ grassCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

{-
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ grassCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html"
              archiveCtx
              >>= loadAndApplyTemplate "templates/default.html"
              archiveCtx
              >>= relativizeUrls -}
------------------------------------------------------------------------
