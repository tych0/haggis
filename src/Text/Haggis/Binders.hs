{- | Contains binding utility functions. -}
module Text.Haggis.Binders (
  -- * Create a transformer which will bind the 'Page' to a template.
  bindPage,
  -- * Bind the specified tag to an anchor.
  bindTag,
  -- * Bind the archives and tags.
  bindSpecial,
  bindComment
  ) where

import Data.Either

import System.FilePath

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
import Text.Haggis.Types
import Text.Haggis.Utils
import Text.Hquery
import Text.XmlHtml

bindPage :: HaggisConfig -> Page -> [Node] -> [Node]
bindPage config Page { pageTitle = title
                     , pageAuthor = author
                     , pageTags = tags
                     , pageDate = date
                     , pagePath = path
                     , pageContent = content
                     , pageComments = comments
                     } =
  let bindTags = if null tags
                   then hq ".tags" nothing
                   else hq ".tag *" (map (bindTag config) tags)
      auth = maybe (defaultAuthor config) Just author
      commentCount = hq ".count *" ((show . length) comments)
  in hq ".title *" title .
     bindTags .
     hq ".author *" auth .
     hq ".date *" (fmap show date) .
     (hq ".content *" $ Group content) .
     hq ".more [href]" (sitePath config </> path) .
     hq ".comments *" (commentCount : map bindComment comments)

bindComment :: Comment -> [Node] -> [Node]
bindComment c = hq ".name *" (commenterName c)
              . hq ".name [href]" (commenterUrl c)
              . hq ".payload *" (pandocToHtml (readMarkdown def (commentPayload c)))
              . hq ".datetime *" (show (commentTime c))

bindTag :: HaggisConfig -> String -> [Node] -> [Node]
bindTag config t = hq "a [href]" (sitePath config </> (mpTypeToPath $ Tag t)) .
                   hq "a *" (t ++ ", ")

bindSpecial :: HaggisConfig -> [MultiPage] -> [Node] -> [Node]
bindSpecial config mps = let (archives, tags) = bindAggregates
                         in hq ".tag" tags . hq ".archive *" archives
  where
    bindAggregates :: ([[Node] -> [Node]], [[Node] -> [Node]])
    bindAggregates = let bind (MultiPage _ typ@(Archive y (Just m))) = Left $
                           hq "a [href]" (sitePath config </> mpTypeToPath typ) .
                           hq "a *" (show y ++ " - " ++ show m)
                         bind (MultiPage xs typ@(Tag t)) = Right $
                           hq ".tag [href]" (sitePath config </> mpTypeToPath typ) .
                           hq ".tag *" (t ++ " (" ++ show (length xs) ++ "), ")
                         bind _ = Left $ hq "*" nothing
                     in partitionEithers $ map bind mps

