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
import Text.Haggis.Types hiding (bindPage, bindComment, bindTag, bindSpecial)
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
                   else hq ".tag *" $ addCommas (map (bindTag config) tags)
      auth = maybe (defaultAuthor config) Just author
  in hq ".title *" title .
     bindTags .
     hq ".author *" auth .
     hq ".date *" (fmap show date) .
     (hq ".content *" $ Group content) .
     hq ".more [href]" (sitePath config </> path) .
     hq ".commentCount *" ((show . length) comments) .
     hq ".comment *" (map bindComment comments)

bindComment :: Comment -> [Node] -> [Node]
bindComment c = nameBind (commenterUrl c)
              . hq ".datetime *" (show (commentTime c))
              . hq ".payload *" (pandocToHtml (readMarkdown readOpts (commentPayload c)))

  where
    nameBind (Just url) = hq ".name *" (commenterName c) . hq ".name [href]" url
    nameBind Nothing = hq ".name" (commenterName c)

bindTag :: HaggisConfig -> String -> [Node] -> [Node]
bindTag config t = hq "a [href]" (sitePath config </> (mpTypeToPath $ Tag t)) .
                   hq "a *" (t)

addCommas :: [[Node] -> [Node]] -> [[Node] -> [Node]]
addCommas ns | not (null ns) = let l = last ns
                                   is = init ns
                               in map ((.) $ hq "* +" ", ") is ++ [l]
addCommas ns = ns

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
                           hq ".tag *" (t ++ " (" ++ show (length xs) ++ ")")
                         bind _ = Left $ hq "*" nothing
                         (archives, tags) = partitionEithers $ map bind mps
                     in (archives, addCommas tags)

