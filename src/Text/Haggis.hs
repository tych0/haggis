module Text.Haggis (
  -- * Site generation entry point
  buildSite
  ) where

import Blaze.ByteString.Builder

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

import Text.Haggis.Parse
import Text.Haggis.Types
import Text.Hquery

bindPage :: Page -> [Node] -> [Node]
bindPage Page { pageTitle = title
              , pageTags = tags
              , pageDate = date
              , pageContent = content
              } = hq ".title *" title .
                  (if null tags then hq ".tags" nothing else hq ".tag" tags) .
                  -- TODO: what if author but no date?
                  maybe (hq ".byline" nothing) (hq ".author") (fmap show date) .
                  hq ".date *" (fmap show date) .
                  (hq ".content *" $ Group content)

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
    wrapper = bindSidebar ps mps $ root templates
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

bindSidebar :: [Page] -> [MultiPage] -> [Node] -> [Node]
bindSidebar ps mps = let (archives, tags) = bindAggregates
                     in bindRecent . hq ".tags" tags . foldl (.) id archives
  where
    bindRecent :: [Node] -> [Node]
    bindRecent = let hasDate = filter (isJust . pageDate) ps
                     recent = take 10 $ sortBy (compare `on` pageDate) hasDate
                     bind p = hq "a [href]" ("/" </> pagePath p) .
                              hq "a *" (pageTitle p) .
                              hq ".author *" (pageAuthor p)
                 in hq ".recentPost *" (map bind recent)
    bindAggregates :: ([[Node] -> [Node]], [[Node] -> [Node]])
    bindAggregates = let bind (MultiPage xs typ@(Archive y (Just m))) = Left $
                           hq ".archive *" (hq "a [href]" (mpTypeToPath typ) .
                                          hq "a *" (show y ++ " - " ++ show m))
                         bind (MultiPage _ typ@(Tag t)) = Right $
                           hq ".tag [href]" (mpTypeToPath typ) .
                           hq ".tag *" (t ++ ", ")
                         bind _ = Left id
                     in partitionEithers $ map bind mps

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
          target = replaceExtension (mkRelative path) ".html"
      parsePage path target
    makeAction info | isRegularFile $ infoStatus info = Left $ do
      let path = infoPath info
      let target = tgt </> mkRelative path
      ensureDirExists target
      copyFile path target
    makeAction _ = Left (return ()) -- TODO: follow symlinks?
