module Text.Haggis (
  -- * Site generation entry point
  buildSite
  ) where

import Blaze.ByteString.Builder

import Control.Applicative

import qualified Data.ByteString.Lazy as BS
import Data.Either
import qualified Data.Map as M
import Data.Maybe
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

data MultiPageType =
  Tag String |
  DirIndex FilePath |
  Archive Integer (Maybe Int)

mpTypeToPath :: MultiPageType -> FilePath
mpTypeToPath (Tag t) = "tags" </> t <.> "html"
mpTypeToPath (DirIndex d) = d </> "index.html"
mpTypeToPath (Archive y m) =
  let month = fromMaybe "" $ fmap show m
  in "archives" </> show y </> month <.> "html"

mpTypeToTitle :: MultiPageType -> String
mpTypeToTitle (Tag t) = "Tagged: " ++ t
mpTypeToTitle (DirIndex d) = "Filed under: " ++ d
mpTypeToTitle (Archive y m) =
  let month = fromMaybe "" $ fmap ((" - " ++) . show) m
  in "Posts from: " ++ (show y) ++ month

data MultiPage = MultiPage {
  pages :: [Page],
  multiPageType :: MultiPageType
}

bindPage :: Page -> [Node] -> [Node]
bindPage Page { pageTitle = title
              , pageTags = tags
              , pageDate = date
              , pageContent = content
              } = hq ".title" title .
                  hq ".tags" tags .
                  hq ".date" (fmap show date) .
                  hq ".content" content

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
  let (multiPages, tags) = generateIndexesAndTags readPages
  ps <- sequence pages
  writeSite ps multiPages templates tags
  sequence_ raws

writeSite :: [Page] -> [MultiPage] -> SiteTemplates -> ([Node] -> [Node]) -> IO ()
writeSite ps mps templates tags = do
  sequence_ $ map writePage ps
  sequence_ $ map writeMultiPage mps
  where
    wrapper = tags $ root templates
    writeThing fp title ns = do
      let xform = hq "#content" ns . hq "#title" title
          html = xform $ wrapper
      ensureDirExists fp
      BS.writeFile fp $ toLazyByteString $ renderHtmlFragment UTF8 html
    writePage :: Page -> IO ()
    writePage p =
      let content = bindPage p $ single templates
      in writeThing (pagePath p) (pageTitle p) content
    writeMultiPage :: MultiPage -> IO ()
    writeMultiPage mp =
      let xform = hq ".page" $ map bindPage $ pages mp
          content = xform $ multiple templates
          path = mpTypeToPath $ multiPageType mp
      in writeThing path (mpTypeToTitle $ multiPageType mp) content

ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . dropFileName

generateIndexesAndTags :: [Page] -> ([MultiPage], [Node] -> [Node])
generateIndexesAndTags ps =
  let tagPages = buildMultiPage Tag tags
      indexPages = buildMultiPage DirIndex indexes
      tagCounts = M.toList $ M.map length tags
      tagBind = hq ".tag" (map (\(t, c) -> t ++ " (" ++ (show c) ++ ")") tagCounts)
  in (tagPages ++ indexPages, tagBind)
  where
    mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
    mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty
    tags = mapAccum $ concatMap (\p -> zip (pageTags p) (repeat p)) ps
    indexes = mapAccum $ map (\p -> (dropFileName $ pagePath p, p)) ps
    buildMultiPage :: (String -> MultiPageType) -> M.Map String [Page] -> [MultiPage]
    buildMultiPage typeBuilder ps =
      map (\(name, pages) -> MultiPage pages $ typeBuilder name) $ M.toList ps

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
      return $ buildPage target html
    makeAction info _ | isRegularFile $ infoStatus info = Left $ do
      let path = infoPath info
      ensureDirExists path
      copyFile path (mkTgtName path)
    makeAction _ _ = Left (return ()) -- TODO: follow symlinks?
