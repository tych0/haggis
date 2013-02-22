module Text.Haggis (
  -- * Site generation entry point
  buildSite
  ) where

import Control.Applicative

import qualified Data.ByteString.Lazy as BS
import Data.Either
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Calendar

import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find
import System.Directory

import Text.XmlHtml

import Text.Haggis.Binders
import Text.Haggis.Config
import Text.Haggis.Parse
import Text.Haggis.RSS
import Text.Haggis.Types
import Text.Haggis.Utils
import Text.Hquery

readTemplates :: FilePath -> IO SiteTemplates
readTemplates fp = SiteTemplates <$> readTemplate (fp </> "root.html")
                                 <*> readTemplate (fp </> "single.html")
                                 <*> readTemplate (fp </> "multiple.html")
                                 <*> readTemplate (fp </> "tags.html")
                                 <*> readTemplate (fp </> "archives.html")

buildSite :: FilePath -> FilePath -> IO ()
buildSite src tgt = do
  templates <- readTemplates $ src </> "templates"
  config <- parseConfig $ src </> "haggis.conf"
  actions <- collectSiteElements (src </> "src") tgt
  let (raws, pages) = partitionEithers actions
  readPages <- sequence pages
  generateRSS config readPages tgt
  let multiPages = generateAggregates readPages
      specialPages = generateSpecial templates multiPages
      allPages = concat [readPages, specialPages]
  writeSite allPages multiPages templates tgt
  sequence_ raws

writeSite :: [Page] -> [MultiPage] -> SiteTemplates -> FilePath -> IO ()
writeSite ps mps templates out = do
  sequence_ $ map writePage ps
  sequence_ $ map writeMultiPage mps
  where
    wrapper = bindSpecial mps $ root templates
    writeThing fp title ns = do
      let xform = hq "#content *" (Group ns) . hq "title *" title
          html = xform $ wrapper
          path = out </> fp
      ensureDirExists path
      BS.writeFile path $ renderHtml html
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

ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . dropFileName

generateSpecial :: SiteTemplates -> [MultiPage] -> [Page]
generateSpecial templates mps =
  let bind = bindSpecial mps
      archivesContent = bind (archivesTemplate templates)
      archives = plainPage "Archives" "./archives/index.html" archivesContent
      tagsContent = bind (tagsTemplate templates)
      tags = plainPage "Tags" "./tags/index.html" tagsContent
  in [archives, tags]
  where
    plainPage :: String -> FilePath -> [Node] -> Page
    plainPage title fp content = Page title Nothing [] Nothing fp content

generateAggregates :: [Page] -> [MultiPage]
generateAggregates ps =
  let tagPages = buildMultiPages Tag tags
      indexPages = buildMultiPages DirIndex indexes
      yearPages = buildMultiPages id yearArchives
      monthPages = buildMultiPages id monthArchives
      rootIndex = MultiPage recent (DirIndex "./")
  in rootIndex : concat [tagPages, indexPages, yearPages, monthPages]
  where
    mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
    mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty
    tags = mapAccum $ concatMap (\p -> zip (pageTags p) (repeat p)) ps
    indexes = let noroot = filter ((/=) "./" . dropFileName . pagePath) ps
              in mapAccum $ map (\p -> (dropFileName $ pagePath p, p)) noroot
    monthAndYearOf d = let (y, m, _) = toGregorian d in (y, Just m)
    yearOf d = let (y, _) = monthAndYearOf d in (y, Nothing)
    buildArchive f =
      mapAccum $ catMaybes $ map (\p -> fmap (\d -> (uncurry Archive $ f d, p)) $ pageDate p) ps
    yearArchives = buildArchive yearOf
    monthArchives = buildArchive monthAndYearOf
    buildMultiPages :: (a -> MultiPageType) -> M.Map a [Page] -> [MultiPage]
    buildMultiPages typeBuilder pm =
      map (\(name, pages) -> MultiPage pages $ typeBuilder name) $ M.toList pm
    recent = let hasDate = filter (isJust . pageDate) ps
             in take 10 $ reverse $ sortBy (compare `on` pageDate) hasDate

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
          target = replaceExtension (mkRelative path) ".html"
      parsePage path target
    makeAction info | isRegularFile $ infoStatus info = Left $ do
      let path = infoPath info
      let target = tgt </> mkRelative path
      ensureDirExists target
      copyFile path target
    makeAction _ = Left (return ()) -- TODO: follow symlinks?
