module Text.Haggis.Parse where

import Data.Char

import Text.Blaze.Renderer.XmlHtml
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options

import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find

import Text.XmlHtml

isSupportedExt :: String -> Bool
isSupportedExt s = (map toLower s) `elem` [".md", ".gtf"]

supported :: FileInfo -> Bool
supported info = (isRegularFile . infoStatus) info &&
                 (isSupportedExt . takeExtension . infoPath) info

renderContent :: FilePath -> IO [Node]
renderContent fp = do
  s <- readFile fp
  return $ renderHtmlNodes $ writeHtml def $ readMarkdown def s
