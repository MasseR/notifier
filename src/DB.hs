{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module DB
  ( HasDB(..)
  , setup
  , WithDB
  , seen
  , setSeen
  , forUnseen
  , SQL.Connection
  , SQL.withConnection
  ) where

import Data.ByteString (ByteString)
import           Control.Monad.Reader
import Control.Monad (unless)
import           Control.Monad.Trans    (MonadIO)
import qualified Database.SQLite.Simple as SQL

class HasDB a where
  getConnection :: a -> SQL.Connection

type WithDB env m = (MonadReader env m, HasDB env, MonadIO m)

setup :: WithDB env m => m ()
setup = execute_ "create table if not exists items (id text primary key)"

query :: (SQL.FromRow r, SQL.ToRow q, WithDB env m) => SQL.Query -> q -> m [r]
query q params = do
  conn <- asks getConnection
  liftIO (SQL.query conn q params)

execute :: (SQL.ToRow q, WithDB env m) => SQL.Query -> q -> m ()
execute q params = do
  conn <- asks getConnection
  liftIO (SQL.execute conn q params)

execute_ :: WithDB env m => SQL.Query -> m ()
execute_ q = do
  conn <- asks getConnection
  liftIO (SQL.execute_ conn q)

seen :: WithDB env m => ByteString -> m Bool
seen key = not . null <$> query @(SQL.Only ByteString) q (SQL.Only key)
  where
    q = "select id from items where id = ?"

setSeen :: WithDB env m => ByteString -> m ()
setSeen key = execute "insert or replace into items (id) values (?)" (SQL.Only key)

forUnseen :: WithDB env m => ByteString -> m () -> m ()
forUnseen key action = do
  s <- seen key
  unless s action
  setSeen key

