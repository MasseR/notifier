module App
  ( App(..)
  ) where

import           Config
import           DB
import           Notify

data App =
  App { notifier   :: Client
      , connection :: Connection
      , config     :: Config }

instance HasNotify App where
  getClient = notifier

instance HasDB App where
  getConnection = connection

