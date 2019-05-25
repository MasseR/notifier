module App
  ( App(..)
  ) where

import Notify
import DB

data App =
  App { notifier :: Client
      , connection :: Connection }

instance HasNotify App where
  getClient = notifier

instance HasDB App where
  getConnection = connection

