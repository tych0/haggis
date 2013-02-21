module Text.Haggis.RSS where

import Control.Exception

import Data.Maybe
import Data.Time.Clock

import Network.URI

import Text.Haggis.Types
import Text.Haggis.Parse
import Text.Haggis.Utils
import Text.RSS

import System.FilePath

generateRSS :: [Page] -> FilePath -> IO ()
generateRSS ps target = writeFile (target </> "rss.xml") feed
  where
    feed = showXML $ rssToXML $ RSS "Blog" uri "A blog" [] $ map buildItem ps
    uri = fromMaybe (throw $ ParseException "bad uri") $ parseURI "http://tycho.ws"
    buildItem :: Page -> Item
    buildItem p = concat [basic, tags, date]
      where
        basic = [ Title (pageTitle p)
                , Link (uri { uriPath = (uriPath uri) </> pagePath p })
                , Description (show $ renderHtml $ pageContent p)
                ]
        tags = map (Category Nothing) $ pageTags p
        date = maybeToList $ fmap (\d -> PubDate (UTCTime d midnight)) (pageDate p)
        midnight = secondsToDiffTime 0
