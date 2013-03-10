module Text.Haggis.Types (
  -- * A haggis configuration.
  HaggisConfig(..),
  -- * Container for the required site templates
  SiteTemplates(..),
  -- * Representation of a single haggis page
  Page(..),
  -- * Representation of multiple pages (e.g. an index.html for a directory,
  --   or a list of posts with a specific tag)
  MultiPage(..),
  -- * The type of the pages
  MultiPageType(..),
  -- ** conversions for 'MultiPageType's
  mpTypeToPath,
  mpTypeToTitle,
  -- * A comment on a haggis post
  Comment(..)
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime () -- for Show UTCTime
import Data.Maybe

import System.FilePath

import Text.XmlHtml

data SiteTemplates = SiteTemplates {
  root :: [Node],
  single :: [Node],
  multiple :: [Node],
  tagsTemplate :: [Node],
  archivesTemplate :: [Node]
} deriving (Show)

data Page = Page {
  pageTitle :: String,
  pageAuthor :: Maybe String,
  pageTags :: [String],
  pageDate :: Maybe Day,
  -- The 'pagePath' is relative to the source dir.
  pagePath :: FilePath,
  pageComments :: [Comment],
  -- This is the content as rendered by pandoc (i.e. it has not been bound to
  -- the single.html template)
  pageContent :: [Node]
} deriving (Show)

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
} deriving (Show)

data HaggisConfig = HaggisConfig {
  -- | Path to where the files are hosted, e.g: /foo, /, /foo/bar/, defaults
  -- to /
  sitePath :: String,

  -- | Default author, so you don't have to put an author in every post's
  -- metadata.
  defaultAuthor :: Maybe String,

  -- | Hostname where the blog is hosted, used for generating RSS feed links.
  -- E.g. blog.example.com
  siteHost :: Maybe String,
  rssTitle :: Maybe String,
  rssDescription :: Maybe String,

  -- | Sqlite3 file name, for comments.
  sqlite3File :: Maybe FilePath,

  siteTemplates :: SiteTemplates
} deriving (Show)

data Comment = Comment {
  commenterName :: String,
  commenterUrl :: Maybe String,
  commenterEmail :: Maybe String,
  commentPayload :: String,
  commentTime :: UTCTime
} deriving (Show)
