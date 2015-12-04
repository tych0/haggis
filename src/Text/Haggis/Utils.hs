module Text.Haggis.Utils where

import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Text as T

import Text.Blaze.Renderer.XmlHtml
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Writers.HTML
import Text.XmlHtml

renderHtml :: [Node] -> BS.ByteString
renderHtml = toLazyByteString . renderHtmlFragment UTF8

mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty

pandocToHtml :: Either PandocError Pandoc -> [Node]
pandocToHtml (Left err) = [TextNode (T.pack (show err))]
pandocToHtml (Right doc) = renderHtmlNodes $ writeHtml writeOpts doc

readOpts :: ReaderOptions
readOpts = def {
  readerExtensions = S.fromList [ Ext_raw_html ],
  readerParseRaw = True,
  readerSmart = True,
  readerTrace = True
}

writeOpts :: WriterOptions
writeOpts = def {
  writerExtensions = S.fromList [ Ext_raw_html ]
}
