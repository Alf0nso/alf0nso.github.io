{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                (liftM)
import           Data.Monoid                  (mappend)
import           Data.Maybe
import qualified Data.Map.Strict        as Map
import           Hakyll
import           Hakyll.Core.Compiler
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Options
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk             (walk, walkM)
import           System.FilePath
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath.Posix        (takeDirectory)

-- | Helper functions --------------------------------------
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

-- Parametric biblio processor
processBib :: Item CSL -> Item Biblio -> Item Pandoc -> Compiler (Item Pandoc)
processBib csl bib pandoc = do
  pandoc' <- withItemBody
      (\(Pandoc (Meta meta) bs) -> pure $
        Pandoc (Meta $ Map.insert "link-citations" (MetaBool True) meta)
        bs)
      pandoc
  fmap insertRefHeading <$> processPandocBiblio csl bib pandoc'
  where
    insertRefHeading :: Pandoc -> Pandoc
    insertRefHeading = walk $ concatMap \case
      d@(Div ("refs", _, _) _) ->
        [Header 2 ("references", [], []) [Str "References"], d]
      block -> [block]

baseFolderAndHtml :: Routes
baseFolderAndHtml =
  customRoute $ takeFileName . (`replaceExtension` "html") . toFilePath

folderAndHtml :: FilePath -> Routes
folderAndHtml path =
    customRoute $
    (++) path . takeFileName . (`replaceExtension` "html") . toFilePath

-- Functions to process file paths
orgToHtml :: FilePath -> FilePath
orgToHtml fp = base ++ (file `replaceExtension` "html")
  where
    (base, file) = splitFileName fp

switchMainFolderFor :: FilePath -> Routes
switchMainFolderFor path =
  customRoute $ (++) path . takeFileName . toFilePath

removeMainFolder :: Routes
removeMainFolder = gsubRoute "main/" (const "")

noMainAndHtml :: Routes
noMainAndHtml = composeRoutes (customRoute $ orgToHtml . toFilePath) removeMainFolder

-- Function to filter drafts, to avoid compiling and uploading them
filterDrafts :: Metadata -> Bool
filterDrafts meta = maybe True (=="false") $ lookupString "draft" meta

-- The compilers I use to generate the website
grassBiblioCompiler :: Compiler (Item String)
grassBiblioCompiler = do
  csl <- load "bib/acm.csl"    :: Compiler (Item CSL)
  bib <- load "bib/global.bib" :: Compiler (Item Biblio)  
  getResourceBody
    >>= readPandocWith defaultHakyllReaderOptions
    >>= processBib csl bib
    >>= pure . (writePandocWith pandocWOptions)

grassCompiler :: Compiler (Item String)
grassCompiler = pandocBiblioCompiler "bib/acm.csl" "bib/global.bib"
------------------------------------------------------------


-- | Configurations and data to use after ------------------
config :: Configuration
config = defaultConfiguration
         { destinationDirectory = "docs"
         , previewPort          = 5000 }

postCtx :: Context String
postCtx = dateField "published" "%B %e, %Y"
          <> defaultContext

posts :: Context String
posts = listField "posts" postCtx $
        do posts <- recentFirst =<< loadAll "main/posts/*/*"
           return posts

archiveCtx :: Context String
archiveCtx = posts
             <> defaultContext

-- Include MathJax in the pandoc options so it is possible
-- to render pretty math equations.
pandocWOptions :: WriterOptions
pandocWOptions = defaultHakyllWriterOptions
                { writerHTMLMathMethod = MathJax "" }
------------------------------------------------------------


buildSite :: IO ()
buildSite = hakyllWith config $ do
  match "bib/global.bib" $ compile biblioCompiler
  match "bib/acm.csl"    $ compile cslCompiler  
  match "templates/*"    $ compile templateBodyCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match ("main/images/**.png" .||. "main/images/**.svg") $ do
    route   $ removeMainFolder
    compile copyFileCompiler

  {- moving pdfs and extra info into a docs folder-}
  match "main/extra/*" $ do
    route   $ switchMainFolderFor "docs/"
    compile copyFileCompiler

  match "main/js/*" $ do
    route   $ removeMainFolder
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

  -- All posts will devided by language folders for now.
  -- 
  matchMetadata "main/posts/*/**.org" filterDrafts $ do
    route   $ noMainAndHtml
    compile $ grassBiblioCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  -- Presentations
  match "main/presentations/*/style.css" $ do
    route   removeMainFolder
    compile compressCssCompiler

  match ("main/presentations/*/**.html"
         .||. "main/presentations/*/**.png"
         .||. "main/presentations/*/**.jpeg"
        ) $ do
    route   $ removeMainFolder
    compile copyFileCompiler

main :: IO ()
main = buildSite
