module Text.Haggis (
  -- * Site generation entry point
  buildSite
  ) where

import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as BS

import System.Posix.Files.ByteString
import System.FilePath
import System.FilePath.Find
import System.Directory

import Text.XmlHtml

import Text.Haggis.Parse

buildSite :: String -> String -> IO [IO ()]
buildSite src tgt = do
  actions <- foldWithHandler
    ignoreExceptions
    always
    (\acc info -> makeAction info : acc)
    []
    src
  return $ reverse actions
  where
    ignoreExceptions _ a _ = return a
    mkTgtName :: FilePath -> FilePath
    mkTgtName = (</>) tgt . makeRelative src
    makeAction :: FileInfo -> IO ()
    makeAction info | (isDirectory . infoStatus) info =
      (createDirectoryIfMissing True . mkTgtName . infoPath) info
    makeAction info | supported info = do
      html <- (renderContent . infoPath) info
      let target = (mkTgtName . infoPath) info
          frag = renderHtmlFragment UTF8 html
      BS.writeFile target $ toLazyByteString frag
    makeAction info | (isRegularFile . infoStatus) info =
      let path = infoPath info in
      copyFile path (mkTgtName path)
    makeAction _ = return ()
