module Text.Haggis.Types (
  SiteTemplates(..),
  Page(..),
  MultiPage(..),
  MultiPageType(..),

  mpTypeToPath,
  mpTypeToTitle
  ) where

import Data.Time.Calendar
import Data.Maybe

import System.FilePath

import Text.XmlHtml

data SiteTemplates = SiteTemplates {
  root :: [Node],
  single :: [Node],
  multiple :: [Node]
}

data Page = Page {
  pageTitle :: String,
  pageAuthor :: Maybe String,
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
