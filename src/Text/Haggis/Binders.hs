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
import Text.Hquery
import Text.XmlHtml

bindPage :: Page -> [Node] -> [Node]
bindPage Page { pageTitle = title
              , pageTags = tags
              , pageDate = date
              , pagePath = path
              , pageContent = content
              } = hq ".title *" title .
                  (if null tags then hq ".tags" nothing else hq ".tag *" (map bindTag tags)) .
                  hq ".date *" (fmap show date) .
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

