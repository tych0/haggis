{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-} -- TODO: get rid of this?
module Text.Haggis.Parse (
 -- * Exception thrown when some template or user input document fails to
 --   parse
 ParseException(..),

 -- * Is this a pandoc supported file?
 supported,
 -- * Parse a date in YYYY-MM-DD format.
 parseDate,
 -- * Utility functions for reading templates
 readTemplate,
 parseHtmlString,
 parsePage,
 dieOnParseError,
 keyValueParser
 ) where

import Control.Applicative hiding (many)
import Control.Exception

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.String
import Data.Text.Internal
import Data.Time.Calendar
import Data.Time.Format

import System.Directory
import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find
import System.Locale hiding (defaultTimeLocale)

import Text.Pandoc.Class hiding (FileInfo)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Parsec
import Text.Parsec.String
import Text.Haggis.Types
import Text.Haggis.Utils
import Text.XmlHtml

data ParseException = ParseException String deriving (Show, Typeable)
instance Exception ParseException

fileTypes :: Map.Map String (Text -> PandocIO Pandoc)
fileTypes = Map.fromList [ (".md", readMarkdown def)
                         ]

isSupportedExt :: String -> Bool
isSupportedExt s = Map.member (map toLower s) fileTypes

supported :: FileInfo -> Bool
supported info = (isRegularFile . infoStatus) info &&
                 (isSupportedExt . takeExtension . infoPath) info

parseDate :: String -> Maybe Day
parseDate = parseTime defaultTimeLocale "%F"

readTemplate :: FilePath -> IO [Node]
readTemplate fp = do
  inp <- readFile fp
  return $ parseHtmlString inp

parseHtmlString :: String -> [Node]
parseHtmlString s = let parseResult = parseHTML "string" (BS.pack s)
                    in either (throw . ParseException) docContent parseResult

parsePage :: FilePath -> FilePath -> [Comment] -> IO Page
parsePage fp target comments = do
  (pageBuilder, content) <- findMetadata
  let Just reader = Map.lookup (takeExtension fp) fileTypes
  doc <- runIO $ reader $ fromString content
  return $ pageBuilder $ pandocToHtml doc
  where
    findMetadata :: IO ([Node] -> Page, String)
    findMetadata = do
      externalMd <- doesFileExist $ fp <.> "meta"
      if externalMd
      then do
        mdf <- readFile $ fp <.> "meta"
        let md = dieOnParseError mdf $ parse keyValueParser "" mdf
        contents <- readFile fp
        return (buildPage md, contents)
      else do
        contents <- readFile fp
        let (md, content) = dieOnParseError fp $ parse inFileMetadata "" contents
        return $ (buildPage $ fromMaybe [] md, content)
    buildPage :: [(String, String)] -> [Node] -> Page
    buildPage md = let m = Map.fromList md
                       title = fromMaybe "" $ Map.lookup "title" m
                       author = Map.lookup "author" m
                       tags = fromMaybe [] $ fmap (splitOn ", ") $ Map.lookup "tags" m
                       date = Map.lookup "date" m >>= parseTime defaultTimeLocale "%F"
                   in Page title author tags date target comments

dieOnParseError :: Show e => String -> Either e a -> a
dieOnParseError prefix (Left m) = throw $ ParseException (prefix ++ show m)
dieOnParseError _ (Right t) = t

inFileMetadata :: Parser (Maybe [(String, String)], String)
inFileMetadata = (,) <$> optionMaybe (string "---" *> newline *> keyValueParser <* string "---")
                     <*> many anyChar

keyValueParser :: Parser [(String, String)]
keyValueParser = many keyValuePair
  where
    keyValuePair :: Parser (String, String)
    keyValuePair = (,) <$> (many alphaNum <* string ":" <* spaces)
                       <*> many (satisfy ((/=) '\n') <?> "printable") <* newline
