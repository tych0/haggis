module Text.Haggis.Utils where

import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as BS

import Text.XmlHtml

renderHtml :: [Node] -> BS.ByteString
renderHtml = toLazyByteString . renderHtmlFragment UTF8
