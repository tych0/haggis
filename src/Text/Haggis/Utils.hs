module Text.Haggis.Utils where

import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Text.Blaze
import Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Utf8 as PHTML
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Options
import Text.Pandoc.Writers.HTML
import Text.XmlHtml

renderHtml :: [Node] -> BSL.ByteString
renderHtml = toLazyByteString . renderHtmlFragment UTF8

mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty

pandocToHtml :: Either PandocError Pandoc -> [Node]
-- Text.Blaze.Renderer.XmlHtml's renderHtmlNodes doesn't render RawNodes in
-- pandoc correctly. Instead, we render the document to a string, and re-parse
-- it with XmlHtml's parseHTML, which parses them correctly into Nodes.
pandocToHtml (Left err) = [TextNode (T.pack (show err))]
pandocToHtml (Right doc) =
  let rendered = PHTML.renderHtml $ pandocMonadToHtml $ runPure $ writeHtml5 def doc
      result = parseHTML "foo" $ BSL.toStrict rendered
  in case result of
       Left err -> [TextNode (T.pack (show err))]
       Right doc2 -> docContent doc2
  where
    pandocMonadToHtml (Left err) = toHtml $ toMarkup $ T.pack (show err)
    pandocMonadToHtml (Right html) = html
