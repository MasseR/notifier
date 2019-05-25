{-# LANGUAGE ConstraintKinds #-}
module Notify
  ( HasNotify(..)
  , WithNotify
  , Message(..)
  , Client
  , notify
  , withSession )
  where

import           Control.Monad.Reader
import           Control.Monad.Trans  (liftIO)
import           DBus.Notify          (Client, Note (..))
import qualified DBus.Notify          as N

class HasNotify a where
  getClient :: a -> Client

type WithNotify env m = (MonadReader env m, HasNotify env, MonadIO m)

newtype Message = Message String

notify :: WithNotify env m => Message -> m ()
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

-- | Run an action with a notify session
--
-- The api doesn't seem to have anything to do with close or disconnect,
-- but I'm making this function for symmetricity sake with DB.hs
withSession :: (Client -> IO a) -> IO a
withSession f = N.connectSession >>= f
