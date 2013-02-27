module Text.Haggis (
  -- * Site generation entry point
  buildSite
  ) where

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

buildSite :: FilePath -> FilePath -> IO ()
buildSite src tgt = do
  templates <- readTemplates $ src </> "templates"
  config <- parseConfig (src </> "haggis.conf") templates
  actions <- collectSiteElements (src </> "src") tgt
  let (raws, pages) = partitionEithers actions
  readPages <- sequence pages
  let multiPages = generateAggregates readPages
      specialPages = generateSpecial config multiPages
      allPages = concat [readPages, specialPages]
  sequence_ raws
  generateRSS config readPages tgt
  writeSite allPages multiPages config tgt

writeSite :: [Page] -> [MultiPage] -> HaggisConfig -> FilePath -> IO ()
writeSite ps mps config out = do
  sequence_ $ map writePage ps
  sequence_ $ map writeMultiPage mps
  where
    wrapper = bindSpecial config mps $ root (siteTemplates config)
    writeThing fp title ns = do
      let xform = hq "#content *" (Group ns) . hq "title *" title
          html = xform $ wrapper
          path = out </> fp
      ensureDirExists path
      BS.writeFile path $ renderHtml html
    writePage :: Page -> IO ()
    writePage p =
      let content = bindPage config p $ single (siteTemplates config)
      in writeThing (pagePath p) (pageTitle p) content
    writeMultiPage :: MultiPage -> IO ()
    writeMultiPage mp =
      let xform = hq ".page *" $ map (bindPage config) $ singlePages mp
          content = xform $ multiple (siteTemplates config)
          path = mpTypeToPath $ multiPageType mp
      in writeThing path (mpTypeToTitle $ multiPageType mp) content

ensureDirExists :: FilePath -> IO ()
ensureDirExists = createDirectoryIfMissing True . dropFileName

generateSpecial :: HaggisConfig -> [MultiPage] -> [Page]
generateSpecial config mps =
  let bind = bindSpecial config mps
      archivesContent = bind (archivesTemplate $ siteTemplates config)
      archives = plainPage "Archives" "./archives/index.html" archivesContent
      tagsContent = bind (tagsTemplate $ siteTemplates config)
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
