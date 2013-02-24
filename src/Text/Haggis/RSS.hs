module Text.Haggis.RSS (
  generateRSS
  ) where

import Control.Exception

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Time.Clock

import Network.URI

import System.FilePath

import Text.Haggis.Config
import Text.Haggis.Types
import Text.Haggis.Utils
import Text.Haggis.Parse
import Text.Hquery
import Text.XmlHtml

template :: [Node]
template = let t = "<rss version=\"2.0\">\
  \<channel><title></title><link></link><description></description></channel>\
  \<item>\
    \<title></title>\
    \<link></link>\
    \<description></description>\
    \<category></category>\
    \<pubDate></pubDate>\
  \</item>\
  \</rss>"
               parseResult = parseXML "template" (BS.pack t)
           in either (throw . ParseException) docContent parseResult

-- | Generate an RSS feed for the pages. Requires that HaggisConfig have a
-- valid "rssTitle", "rssDescription", and "siteHost".
generateRSS :: HaggisConfig -> [Page] -> FilePath -> IO ()
generateRSS conf ps target = fromMaybe (return ()) $ do
  title <- rssTitle conf
  description <- rssDescription conf
  uri <- rootUri conf
  let bindChannel = hq "channel *" ( hq "title *" title
                                   . hq "description *" description
                                   . hq "link *" (show uri)
                                   )
      bindItems = map (buildItem uri) ps
      xml = (bindChannel . hq "item *" bindItems) template
  return $ writeFile (target </> "rss.xml") (show $ renderXml $ xml)
  where
    buildItem :: URI -> Page -> [Node] -> [Node]
    buildItem baseURI p = hq "item *" ( hq "title *" (pageTitle p)
                                      . hq "link *" myURI
                                      . hq "description *" myDescription
                                      . hq "pubDate *" date
                                      . hq "category *" (pageTags p)
                                      )
      where
        -- TODO: Should probably be RFC822 like the spec says... I didn't
        -- try to figure out how to use the types in Data.Time.Format.RFC822
        -- though.
        date = fmap (\d -> show (UTCTime d midnight)) (pageDate p)
        myURI = show (baseURI { uriPath = (uriPath baseURI) </> pagePath p })
        myDescription = let content = show $ renderHtml $ pageContent p
                        in "<![CDATA[" ++ content ++ "]]>"
        midnight = secondsToDiffTime 0
