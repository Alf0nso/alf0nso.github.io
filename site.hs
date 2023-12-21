{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Maybe
import           Hakyll
import           Hakyll.Core.Compiler
import           Text.Pandoc.Options
import           System.FilePath
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath.Posix (takeDirectory)

-- Helper functions
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

-- | My own configuration for the website
config :: Configuration
config = defaultConfiguration
         { destinationDirectory = "docs"
         , previewPort          = 5000 }
-- --------------------------------------

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
          <> defaultContext

posts :: Context String
posts = listField "posts" postCtx $
        do posts <- recentFirst =<< loadAll "main/posts/en/*"
           return posts

archiveCtx :: Context String
archiveCtx = posts
             <> constField "title" "Archives"
             <> defaultContext
             
-- --------------------------------------

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

folderAndHtml :: FilePath -> Routes
folderAndHtml path =
    customRoute $
    (++) path . takeFileName . (`replaceExtension` "html") . toFilePath

removeMainFolder :: FilePath -> Routes
removeMainFolder path =
  customRoute $ (++) path . takeFileName . toFilePath

------------------------------------------------------------------------
buildSite :: IO ()
buildSite = hakyllWith config $ do
  match "templates/*" $ compile templateBodyCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "main/images/*" $ do
    route   $ removeMainFolder "images/"
    compile copyFileCompiler

  {- moving pdfs and extra info into a docs folder-}
  match "main/extra/*" $ do
    route   $ removeMainFolder "docs/"
    compile copyFileCompiler

  match "main/*" $ do
    route     baseFolderAndHtml
    compile $ grassCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "main/Alf0nso/index.org" $ do
    route   $ folderAndHtml "Alf0nso/"
    compile $ grassCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
  
  match "main/posts/archive.org" $ do
    route     baseFolderAndHtml
    compile $ grassCompiler
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  
  match "main/posts/en/*" $ do
    route     baseFolderAndHtml
    compile $ grassCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

main :: IO ()
main = buildSite
