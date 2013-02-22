module Text.Haggis.RSS where

import Data.Maybe
import Data.Time.Clock

import Network.URI

import Text.Haggis.Config
import Text.Haggis.Types
import Text.Haggis.Utils
import Text.RSS

import System.FilePath

generateRSS :: HaggisConfig -> [Page] -> FilePath -> IO ()
generateRSS conf ps target = fromMaybe (return ()) $ do
  title <- rssTitle conf
  desc <- rssDescription conf
  uri <- rootUri conf
  let rss = RSS title uri desc [] $ map (buildItem uri) ps
  return $ writeFile (target </> "rss.xml") (showXML $ rssToXML rss)
  where
    buildItem :: URI -> Page -> Item
    buildItem baseURI p = concat [basic, tags, date]
      where
        basic = [ Title (pageTitle p)
                , Link (baseURI { uriPath = (uriPath baseURI) </> pagePath p })
                , Description (show $ renderHtml $ pageContent p)
                ]
        tags = map (Category Nothing) $ pageTags p
        date = maybeToList $ fmap (\d -> PubDate (UTCTime d midnight)) (pageDate p)
        midnight = secondsToDiffTime 0
