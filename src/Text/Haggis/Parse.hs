{-# LANGUAGE DeriveDataTypeable #-}
module Text.Haggis.Parse where

import Control.Exception

import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Map as Map
import Data.Typeable

import Data.Time.Calendar
import Data.Time.Format

import Text.Blaze.Renderer.XmlHtml
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.Definition

import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find
import System.Locale

import Text.XmlHtml

data ParseException = ParseException String deriving (Show, Typeable)
instance Exception ParseException

fileTypes :: Map.Map String (String -> Pandoc)
fileTypes = Map.fromList [ (".md", readMarkdown def)
                         , (".gtf", readMarkdown def)
                         ]

isSupportedExt :: String -> Bool
isSupportedExt s = Map.member (map toLower s) fileTypes

supported :: FileInfo -> Bool
supported info = (isRegularFile . infoStatus) info &&
                 (isSupportedExt . takeExtension . infoPath) info

parseDate :: String -> Maybe Day
parseDate = parseTime defaultTimeLocale "%F"

renderContent :: FilePath -> IO [Node]
renderContent fp = do
  s <- readFile fp
  let Just reader = Map.lookup (takeExtension fp) fileTypes
  return $ renderHtmlNodes $ writeHtml def $ reader s

readTemplate :: FilePath -> IO [Node]
readTemplate fp = do
  inp <- readFile fp
  let parseResult = parseHTML fp (BS.pack inp)
  return $ either (throw . ParseException) docContent parseResult
