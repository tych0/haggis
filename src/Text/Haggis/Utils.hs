module Text.Haggis.Utils where

import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Lazy as M

import Text.XmlHtml

renderHtml :: [Node] -> BS.ByteString
renderHtml = toLazyByteString . renderHtmlFragment UTF8

mapAccum :: Ord a => [(a, b)] -> M.Map a [b]
mapAccum = foldr (\(k,v) -> M.insertWith (++) k [v]) M.empty
