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
  deriving (Eq, Ord, Show)

mpTypeToPath :: MultiPageType -> FilePath
mpTypeToPath (Tag t) = "tags" </> t <.> "html"
mpTypeToPath (DirIndex d) = d </> "index.html"
mpTypeToPath (Archive y m) =
  let month = fromMaybe "index" $ fmap show m
  in "archives" </> show y </> month <.> "html"

mpTypeToTitle :: MultiPageType -> String
mpTypeToTitle (Tag t) = "Tagged: " ++ t
mpTypeToTitle (DirIndex d) = "Filed under: " ++ d
mpTypeToTitle (Archive y m) =
  let month = fromMaybe "" $ fmap ((" - " ++) . show) m
  in "Posts from: " ++ (show y) ++ month

data MultiPage = MultiPage {
  singlePages :: [Page],
  multiPageType :: MultiPageType
}

bindPage :: Page -> [Node] -> [Node]
bindPage Page { pageTitle = title
              , pageTags = tags
              , pageDate = date
              , pageContent = content
              } = hq ".title *" title .
                  hq ".tags *" tags .
                  hq ".date *" (fmap show date) .
                  (hq ".content *" $ Group content)

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
  actions <- collectSiteElements (src </> "src") tgt
  let (raws, pages) = partitionEithers actions
  readPages <- sequence pages
  let multiPages = generateAggregates readPages
  ps <- sequence pages
  writeSite ps multiPages templates tgt
  sequence_ raws

writeSite :: [Page] -> [MultiPage] -> SiteTemplates -> FilePath -> IO ()
writeSite ps mps templates out = do
  sequence_ $ map writePage ps
  sequence_ $ map writeMultiPage mps
  where
    wrapper = bindSidebar mps $ root templates
    writeThing fp title ns = do
      let xform = hq "#content *" (Group ns) . hq "title *" title
          html = xform $ wrapper
          path = out </> fp
      ensureDirExists path
      BS.writeFile path $ toLazyByteString $ renderHtmlFragment UTF8 html
    writePage :: Page -> IO ()
    writePage p =
      let content = bindPage p $ single templates
      in writeThing (pagePath p) (pageTitle p) content
    writeMultiPage :: MultiPage -> IO ()
    writeMultiPage mp =
      let xform = hq ".page *" $ map bindPage $ singlePages mp
          content = xform $ multiple templates
          path = mpTypeToPath $ multiPageType mp
      in writeThing path (mpTypeToTitle $ multiPageType mp) content

bindSidebar :: [MultiPage] -> [Node] -> [Node]
bindSidebar mps = id
    -- tagCounts = M.toList $ M.map length tags
    -- tagBind = hq ".tag" (map (\(t, c) -> t ++ " (" ++ (show c) ++ ")") tagCounts)

ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . dropFileName

generateAggregates :: [Page] -> [MultiPage]
generateAggregates ps =
  let tagPages = buildMultiPages Tag tags
      indexPages = buildMultiPages DirIndex indexes
      yearPages = buildMultiPages id yearArchives
      monthPages = buildMultiPages id monthArchives
  in concat [tagPages, indexPages, yearPages, monthPages]
  where
    mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
    mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty
    tags = mapAccum $ concatMap (\p -> zip (pageTags p) (repeat p)) ps
    indexes = mapAccum $ map (\p -> (dropFileName $ pagePath p, p)) ps

    monthAndYearOf d = let (y, m, _) = toGregorian d in (y, Just m)
    yearOf d = let (y, _) = monthAndYearOf d in (y, Nothing)
    buildArchive f =
      mapAccum $ catMaybes $ map (\p -> fmap (\d -> (uncurry Archive $ f d, p)) $ pageDate p) ps
    yearArchives = buildArchive yearOf
    monthArchives = buildArchive monthAndYearOf

    buildMultiPages :: (a -> MultiPageType) -> M.Map a [Page] -> [MultiPage]
    buildMultiPages typeBuilder pm =
      map (\(name, pages) -> MultiPage pages $ typeBuilder name) $ M.toList pm

type Accum = [Either (IO ()) (IO Page)]
collectSiteElements ::
  FilePath ->
  FilePath ->
  IO Accum
collectSiteElements src tgt = foldWithHandler
  ignoreExceptions
  always
  accumulate
  []
  src
  where
    accumulate :: Accum -> FileInfo -> Accum
    accumulate acc info = makeAction info : acc
    ignoreExceptions _ a _ = return a
    mkRelative :: FilePath -> FilePath
    mkRelative = makeRelative src
    makeAction :: FileInfo -> Either (IO ()) (IO Page)
    makeAction info | supported info = Right $ do
      let path = infoPath info
      html <- renderContent path
      let target = replaceExtension (mkRelative path) ".html"
      return $ buildPage target html
    makeAction info | isRegularFile $ infoStatus info = Left $ do
      let path = infoPath info
      let target = tgt </> mkRelative path
      ensureDirExists target
      copyFile path target
    makeAction _ = Left (return ()) -- TODO: follow symlinks?
