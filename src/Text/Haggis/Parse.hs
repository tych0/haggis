module Text.Haggis.Parse where

import Data.Char
import qualified Data.Map as Map

import Text.Blaze.Renderer.XmlHtml
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.Definition

import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find

import Text.XmlHtml

fileTypes :: Map.Map String (String -> Pandoc)
fileTypes = Map.fromList [ (".md", readMarkdown def)
                         , (".gtf", readMarkdown def)
                         ]

isSupportedExt :: String -> Bool
isSupportedExt s = Map.member (map toLower s) fileTypes

supported :: FileInfo -> Bool
supported info = (isRegularFile . infoStatus) info &&
                 (isSupportedExt . takeExtension . infoPath) info

renderContent :: FilePath -> IO [Node]
renderContent fp = do
  s <- readFile fp
  let Just reader = Map.lookup (takeExtension fp) fileTypes
  return $ renderHtmlNodes $ writeHtml def $ reader s
