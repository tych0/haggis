{- | Contains binding utility functions. -}
module Text.Haggis.Binders (
  -- * Create a transformer which will bind the 'Page' to a template.
  bindPage,
  -- * Bind the specified tag to an anchor.
  bindTag,
  -- * Bind the archives and tags.
  bindSpecial
  ) where

import Data.Either

import System.FilePath

import Text.Haggis.Types
import Text.Haggis.Config
import Text.Hquery
import Text.XmlHtml

bindPage :: HaggisConfig -> Page -> [Node] -> [Node]
bindPage config Page { pageTitle = title
                     , pageAuthor = author
                     , pageTags = tags
                     , pageDate = date
                     , pagePath = path
                     , pageContent = content
                     } =
  let bindTags = if null tags
                   then hq ".tags" nothing
                   else hq ".tag *" (map bindTag tags)
      auth = maybe (defaultAuthor config) Just author
  in hq ".title *" title .
     bindTags .
     hq ".author *" auth .
     hq ".date *" (fmap show date) .
     (hq ".content *" $ Group content) .
     hq ".more [href]" ("/" </> path)

bindTag :: String -> [Node] -> [Node]
bindTag t = hq "a [href]" ("/" </> (mpTypeToPath $ Tag t)) .
            hq "a *" (t ++ ", ")

bindSpecial :: [MultiPage] -> [Node] -> [Node]
bindSpecial mps = let (archives, tags) = bindAggregates
                  in hq ".tag" tags . hq ".archive *" archives
  where
    bindAggregates :: ([[Node] -> [Node]], [[Node] -> [Node]])
    bindAggregates = let bind (MultiPage _ typ@(Archive y (Just m))) = Left $
                           hq "a [href]" ("/" </> mpTypeToPath typ) .
                           hq "a *" (show y ++ " - " ++ show m)
                         bind (MultiPage xs typ@(Tag t)) = Right $
                           hq ".tag [href]" ("/" </> mpTypeToPath typ) .
                           hq ".tag *" (t ++ " (" ++ show (length xs) ++ "), ")
                         bind _ = Left $ hq "*" nothing
                     in partitionEithers $ map bind mps

