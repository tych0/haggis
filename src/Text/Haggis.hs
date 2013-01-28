module Text.Haggis (
  -- * Site generation entry point
  buildSite
  ) where

import Blaze.ByteString.Builder

import Control.Applicative

import qualified Data.ByteString.Lazy as BS
import Data.Either
import Data.Time.Calendar

import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find
import System.Directory

import Text.XmlHtml

import Text.Haggis.Parse
import Text.Hquery

data SiteTemplates = SiteTemplates {
  root :: [Node],
  single :: [Node],
  multiple :: [Node]
}

data Page = Page {
  pageTitle :: String,
  pageTags :: [String],
  pageDate :: Maybe Day,
  pagePath :: FilePath,
  pageContent :: [Node]
}

buildPage :: FilePath -> [Node] -> Page
buildPage fp ns = Page { pageTitle = "foo"
                       , pageTags = []
                       , pageDate = parseDate "2013-01-28"
                       , pagePath = fp
                       , pageContent = ns
                       }

readTemplates :: FilePath -> IO SiteTemplates
readTemplates fp = SiteTemplates <$> readTemplate (fp </> "root.html")
                                 <*> readTemplate (fp </> "single.html")
                                 <*> readTemplate (fp </> "multiple.html")

buildSite :: String -> String -> IO ()
buildSite src tgt = do
  templates <- readTemplates $ src </> "templates"
  actions <- collectSiteElements src tgt templates
  let (raws, pages) = partitionEithers actions
  readPages <- sequence pages
  sequence_ $ raws ++ map writePage readPages
  where
    writePage :: Page -> IO ()
    writePage p = let path = pagePath p
                      content = pageContent p
                      builder = renderHtmlFragment UTF8 content
                  in BS.writeFile path $ toLazyByteString builder

ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . dropFileName

type Accum = [Either (IO ()) (IO Page)]
collectSiteElements ::
  FilePath ->
  FilePath ->
  SiteTemplates ->
  IO Accum
collectSiteElements src tgt templates = foldWithHandler
  ignoreExceptions
  always
  accumulate
  []
  src
  where
    accumulate :: Accum -> FileInfo -> Accum
    accumulate acc info = makeAction info templates : acc
    ignoreExceptions _ a _ = return a
    mkTgtName :: FilePath -> FilePath
    mkTgtName = (</>) tgt . makeRelative src
    makeAction :: FileInfo -> SiteTemplates -> Either (IO ()) (IO Page)
    makeAction info templates | supported info = Right $ do
      let path = infoPath info
      html <- renderContent path
      let target = replaceExtension (mkTgtName path) ".html"
          page = hq "#content" html $ root templates
      return $ buildPage path page
    makeAction info _ | isRegularFile $ infoStatus info = Left $ do
      let path = infoPath info
      ensureDirExists path
      copyFile path (mkTgtName path)
    makeAction _ _ = Left (return ()) -- TODO: follow symlinks?
