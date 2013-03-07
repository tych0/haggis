{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Text.Haggis.Comments (
  getComments,
  commentsEnabled,
  CommentException(..)
  ) where

import Control.Exception
import Control.Monad

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Traversable as T
import Data.Typeable

import Prelude hiding (catch)

import System.FilePath

import Text.Haggis.Types
import Text.Haggis.Utils

data CommentException = CommentException String deriving (Show, Typeable)
instance Exception CommentException

getComments :: HaggisConfig -> IO (FilePath -> [Comment])
getComments conf = do
  conn <- getConnection conf
  comments <- T.sequence (fmap queryComments conn)
  return $ \fp -> fromMaybe [] $ M.lookup (normalize fp) (fromMaybe M.empty comments)
  where
    -- Comments are stored in the database according to their path relative to
    -- the src/ directory in the tree, so we normalize them accordingly here.
    -- For example, sitePrefix is /foo, then:
    --   /foo/bar/baz.html -> bar/baz
    --   /foo/bar/bonk.html -> bar/bonk
    normalize :: FilePath -> FilePath
    normalize = dropExtension . normalise . makeRelative (sitePath conf)

getConnection :: HaggisConfig -> IO (Maybe ConnWrapper)
getConnection conf = T.sequence $ getConnectionBuilder conf

getConnectionBuilder :: HaggisConfig -> Maybe (IO ConnWrapper)
getConnectionBuilder conf =
  fmap (\c -> liftM ConnWrapper (connectSqlite3 c)) (sqlite3File conf)

commentsEnabled :: HaggisConfig -> Bool
commentsEnabled = isJust . getConnectionBuilder

queryComments :: ConnWrapper -> IO (M.Map FilePath [Comment])
queryComments conn = do
  stmt <- prepare conn "select * from \"comments\";"
  ms <- fetchAllRowsMap' stmt
  return $ mapAccum $ map toComment ms
  where
    toComment :: M.Map String SqlValue -> (FilePath, Comment)
    toComment row =
      let c = Comment
                (get "name")
                (get "url")
                (get "email")
                (get "payload")
                (get "time")
      in ((get "slug"), c)
      where
        get :: Convertible SqlValue a => String -> a
        get col = let v = fmap fromSql (M.lookup col row)
                  in fromMaybe
                       (throw (CommentException ("couldn't find column: " ++ col)))
                       v
