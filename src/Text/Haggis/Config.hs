module Text.Haggis.Config (
  HaggisConfig(..),
  parseConfig,
  rootUri
  ) where

import Control.Exception

import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.String.Utils

import Network.URI

import Prelude hiding (catch)

import System.IO

import Text.Haggis.Parse
import Text.Parsec

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
  rssDescription :: Maybe String
} deriving (Show)

parseConfig :: FilePath -> IO HaggisConfig
parseConfig fp = do
  inp <- catch (readFile fp)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("Problem reading: " ++ err)
                         return "")
  let kvs = dieOnParseError fp $ parse keyValueParser "" inp
  return $ buildConfig $ M.fromList kvs

buildConfig :: M.Map String String -> HaggisConfig
buildConfig kvs = let get = (flip M.lookup) kvs in HaggisConfig
  (fromMaybe "/" $ get "sitePath")
  (get "defaultAuthor")
  (get "siteHost")
  (get "rssTitle")
  (get "rssDescription")

rootUri :: HaggisConfig -> Maybe URI
rootUri c = siteHost c >>= \h -> parseURI $ "http://" ++ h </> sitePath c
  where
    (</>) :: String -> String -> String
    (</>) f s | (endswith "/" f && not (startswith "/" s)) ||
                (not (endswith "/" f) && startswith "/" s) = f ++ s
    (</>) f s | not (endswith "/" f) && not (startswith "/" s) = f ++ "/" ++ s
    (</>) f s = f ++ drop 1 s
