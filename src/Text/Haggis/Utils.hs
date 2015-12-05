module Text.Haggis.Utils where

import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import qualified Text.Blaze.Html.Renderer.Utf8 as PHTML
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Writers.HTML
import Text.XmlHtml

import Debug.Trace

renderHtml :: [Node] -> BSL.ByteString
renderHtml = toLazyByteString . renderHtmlFragment UTF8

mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty

pandocToHtml :: Either PandocError Pandoc -> [Node]
pandocToHtml (Left err) = [TextNode (T.pack (show err))]
-- Text.Blaze.Renderer.XmlHtml's renderHtmlNodes doesn't render RawNodes in
-- pandoc correctly. Instead, we render the document to a string, and re-parse
-- it with XmlHtml's parseHTML, which parses them correctly into Nodes.
pandocToHtml (Right doc) =
  let rendered = PHTML.renderHtml $ writeHtml def doc
      result = parseHTML "foo" $ (trace (show rendered) (BSL.toStrict rendered))
  in case result of
       Left err -> [TextNode (T.pack (show err))]
       Right doc2 -> docContent doc2
