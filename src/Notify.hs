{-# LANGUAGE ConstraintKinds #-}
module Notify
  ( HasNotify(..)
  , WithNotify
  , Message(..)
  , notify )
  where

import           Control.Monad.Reader
import           DBus.Notify          (Client, Note(..))
import qualified DBus.Notify          as N
import Control.Monad.Trans (liftIO)

class HasNotify a where
  getClient :: a -> Client

type WithNotify env m = (MonadReader env m, HasNotify env)

newtype Message = Message String

notify :: (MonadIO m, WithNotify env m) => Message -> m ()
notify (Message m) = do
  client <- asks getClient
  let note = N.Note { appName = "notifier"
                    , appImage = Nothing
                    , summary = m
                    , body = Just (N.Text m)
                    , actions = []
                    , hints = []
                    , expiry = N.Dependent
                    }
  void $ liftIO $ N.notify client note
