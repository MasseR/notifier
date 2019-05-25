module App
  ( App(..)
  ) where

import Notify

newtype App = App { notifier :: Client }

instance HasNotify App where
  getClient = notifier

