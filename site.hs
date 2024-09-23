{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (liftM)
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
postCtx = dateField "published" "%B %e, %Y"
          <> defaultContext

posts :: Context String
posts = listField "posts" postCtx $
        do posts <- recentFirst =<< loadAll "main/posts/en/*"
           return posts

archiveCtx :: Context String
archiveCtx = posts
             <> defaultContext             
-- --------------------------------------

{- Include MathJax in the pandoc options so it is possible
to render pretty math equations. -}
pandocWOptions :: WriterOptions
pandocWOptions = defaultHakyllWriterOptions
                { writerHTMLMathMethod = MathJax "" }

{- My personal website compiler with my preferences. -}
grassBiblioCompiler :: Compiler (Item String)
grassBiblioCompiler = do
  csl <- load "bib/tpls.csl"
  bib <- load "bib/global.bib"
  getResourceBody
    >>= readPandocBiblio defaultHakyllReaderOptions csl bib
    >>= pure . (writePandocWith pandocWOptions)

grassCompiler :: Compiler (Item String)
grassCompiler = pandocBiblioCompiler "bib/acm.csl" "bib/global.bib"

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

-- If there is a draft field and it has a value other than false, then
-- the site generator will not publish it/consider it
filterDrafts :: Metadata -> Bool
filterDrafts meta = maybe True (=="false") $ lookupString "draft" meta
------------------------------------------------------------------------

buildSite :: IO ()
buildSite = hakyllWith config $ do
  match "bib/global.bib" $ compile biblioCompiler
  match "bib/acm.csl" $ compile cslCompiler  
  match "templates/*" $ compile templateBodyCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "main/images/*" $ do
    route   $ removeMainFolder "images/"
    compile copyFileCompiler

  match "main/images/books/*" $ do
    route   $ removeMainFolder "images/books/"
    compile copyFileCompiler

  match "main/images/myself/*" $ do
    route   $ removeMainFolder "images/myself/"
    compile copyFileCompiler

  match "main/images/animals/*" $ do
    route   $ removeMainFolder "images/animals/"
    compile copyFileCompiler

  match "main/images/icons/*" $ do
    route   $ removeMainFolder "images/icons/"
    compile copyFileCompiler

  {- moving pdfs and extra info into a docs folder-}
  match "main/extra/*" $ do
    route   $ removeMainFolder "docs/"
    compile copyFileCompiler

  match "main/js/*" $ do
    route   $ removeMainFolder "js/"
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
  
  matchMetadata "main/posts/en/*" filterDrafts $ do
    route     baseFolderAndHtml
    compile $ grassCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  matchMetadata "main/posts/pt/*" filterDrafts $ do
   route     baseFolderAndHtml
   compile $ grassCompiler
     >>= loadAndApplyTemplate "templates/post.html"    postCtx
     >>= loadAndApplyTemplate "templates/default.html" postCtx
     >>= relativizeUrls

main :: IO ()
main = buildSite
