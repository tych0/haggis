module Text.Haggis.Config (
  parseConfig,
  rootUri,
  readTemplates,
  getBindPage,
  getBindTag,
  getBindComment,
  getBindSpecial,
  ) where

import qualified Control.Exception as E

import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.String.Utils

import Network.URI

import System.FilePath
import System.IO

import Text.Haggis.Parse
import Text.Haggis.Types
import qualified Text.Haggis.Binders as Bind
import Text.Parsec
import Text.XmlHtml

parseConfig :: FilePath -> SiteTemplates -> IO HaggisConfig
parseConfig fp ts = do
  inp <- E.catch (readFile fp)
               (\e -> do let err = show (e :: E.IOException)
                         hPutStr stderr ("Problem reading: " ++ err)
                         return "")
  let kvs = dieOnParseError fp $ parse keyValueParser "" inp
  return $ buildConfig (M.fromList kvs) ts

buildConfig :: M.Map String String
            -> SiteTemplates
            -> HaggisConfig
buildConfig kvs = let get = (flip M.lookup) kvs in HaggisConfig
  (fromMaybe "/" $ get "sitePath")
  (get "defaultAuthor")
  (get "siteHost")
  (get "rssTitle")
  (get "rssDescription")
  (get "sqlite3File")
  (get "indexTitle")
  -- binders
  Nothing
  Nothing
  Nothing
  Nothing

rootUri :: HaggisConfig -> Maybe URI
rootUri c = siteHost c >>= \h -> parseURI $ "http://" ++ pappend h (sitePath c)
  where
    pappend :: String -> String -> String
    pappend f s | (endswith "/" f && not (startswith "/" s)) ||
                (not (endswith "/" f) && startswith "/" s) = f ++ s
    pappend f s | not (endswith "/" f) && not (startswith "/" s) = f ++ "/" ++ s
    pappend f s = f ++ drop 1 s

readTemplates :: FilePath -> IO SiteTemplates
readTemplates fp = SiteTemplates <$> readTemplate (fp </> "root.html")
                                 <*> readTemplate (fp </> "single.html")
                                 <*> readTemplate (fp </> "multiple.html")
                                 <*> readTemplate (fp </> "tags.html")
                                 <*> readTemplate (fp </> "archives.html")

getBindPage :: HaggisConfig -> Page -> [Node] -> [Node]
getBindPage c = fromMaybe (Bind.bindPage c) (bindPage c)

getBindTag :: HaggisConfig -> String -> [Node] -> [Node]
getBindTag c = fromMaybe (Bind.bindTag c) (bindTag c)

getBindComment :: HaggisConfig -> Comment -> [Node] -> [Node]
getBindComment c = fromMaybe Bind.bindComment (bindComment c)

getBindSpecial :: HaggisConfig -> [MultiPage] -> [Node] -> [Node]
getBindSpecial c = fromMaybe (Bind.bindSpecial c) (bindSpecial c)
